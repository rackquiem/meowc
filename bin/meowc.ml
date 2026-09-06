open Meow
open Types

let usage =
  String.concat "\n"
    [
      Style.bold "meowc" ^ Style.dim "  a build tool for C and C++";
      "";
      Style.bold "  usage";
      "    meowc [build] [target...]     build everything, or the named targets";
      "    meowc run <name> [args...]    run a bin target, or a run block";
      "    meowc test [filter]           build and run test targets";
      "    meowc install                 copy installable outputs under the prefix";
      "    meowc configure               run the checks and write the config header";
      "    meowc check                   parse and validate, build nothing";
      "    meowc targets                 list every target";
      "    meowc graph [target...]       show the dependency graph";
      "                                  --show draws it, --dot writes graphviz";
      "    meowc explain <file>          say why a file is or is not up to date";
      "    meowc compdb                  write compile_commands.json";
      "    meowc watch [target...]       rebuild whenever an input changes";
      "    meowc init <name>             scaffold a new project here";
      "    meowc clean                   delete the build directory";
      "";
      Style.bold "  options";
      "    -f FILE        build file to read (default build.meow)";
      "    -j N           parallel jobs (default: cores)";
      "    -C DIR         change to DIR first";
      "    -D name=value  set an option declared with 'option'";
      "    --prefix DIR   install prefix (default /usr/local)";
      "    --target TRIPLE cross compile for TRIPLE (e.g. aarch64-linux-gnu)";
      "    --sysroot DIR  sysroot to compile and link against";
      "    -k             keep going after a failure";
      "    -n             dry run, print what would happen";
      "    -v             print each command as it finishes";
      "    -q             only print warnings and errors";
      "    --no-color     plain output";
    ]

let cores_from_cpuinfo () =
  match Fs.read "/proc/cpuinfo" with
  | exception Sys_error _ -> None
  | s -> (
      match
        List.length
          (List.filter
             (fun l -> String.length l >= 9 && String.sub l 0 9 = "processor")
             (String.split_on_char '\n' s))
      with
      | 0 -> None
      | n -> Some n)

let cores_from_getconf () =
  match Exec.capture [| "getconf"; "_NPROCESSORS_ONLN" |] with
  | 0, out -> int_of_string_opt (String.trim out)
  | _ -> None
  | exception _ -> None

let cores () =
  match cores_from_cpuinfo () with
  | Some n -> n
  | None -> ( match cores_from_getconf () with Some n when n > 0 -> n | _ -> 4)

let duration s = if s >= 1.0 then Printf.sprintf "%.2f s" s else Printf.sprintf "%.0f ms" (s *. 1000.0)

type flags = {
  mutable file : string;
  mutable jobs : int;
  mutable chdir : string;
  mutable defs : (string * string) list;
  mutable prefix : string option;
  mutable keep : bool;
  mutable dry : bool;
  mutable verbose : bool;
  mutable quiet : bool;
  mutable dot : bool;
  mutable show : bool;
  mutable targets_only : bool;
  mutable interval : float;
  mutable target : string;
  mutable sysroot : string;
}

let fl =
  { file = "build.meow"; jobs = 0; chdir = ""; defs = []; prefix = None; keep = false;
    dry = false; verbose = false; quiet = false; dot = false; show = false; targets_only = false;
    interval = 0.25; target = ""; sysroot = "" }

let parse_args argv =
  let rest = ref [] in
  (* Once "run <target>" is on the positional list, the rest belongs to the
     program being run, not to meowc. "--" ends option parsing anywhere. *)
  let passthrough () = match !rest with "run" :: _ :: _ -> true | _ -> false in
  let rec go = function
    | [] -> ()
    | "--" :: r when passthrough () -> rest := !rest @ r
    | args when passthrough () -> rest := !rest @ args
    | "--" :: r -> rest := !rest @ r
    | "-f" :: v :: r -> fl.file <- v; go r
    | "-j" :: v :: r ->
        fl.jobs <- (try int_of_string v with _ -> Diag.error "-j wants a number, got %S" v);
        go r
    | "-C" :: v :: r -> fl.chdir <- v; go r
    | "-D" :: v :: r ->
        (match String.index_opt v '=' with
        | None -> Diag.error ~hint:"write -D name=value" "malformed -D %S" v
        | Some i ->
            fl.defs <- fl.defs @ [ (String.sub v 0 i, String.sub v (i + 1) (String.length v - i - 1)) ]);
        go r
    | "--prefix" :: v :: r -> fl.prefix <- Some v; go r
    | "--target" :: v :: r -> fl.target <- v; go r
    | "--sysroot" :: v :: r -> fl.sysroot <- v; go r
    | "--interval" :: v :: r -> fl.interval <- (try float_of_string v with _ -> 0.25); go r
    | "--dot" :: r -> fl.dot <- true; go r
    | "--show" :: r -> fl.show <- true; go r
    | "--targets" :: r -> fl.targets_only <- true; go r
    | "-k" :: r -> fl.keep <- true; go r
    | "-n" :: r -> fl.dry <- true; go r
    | "-v" :: r -> fl.verbose <- true; go r
    | "-q" :: r -> fl.quiet <- true; go r
    | "--no-color" :: r -> Style.enabled := false; go r
    | ("-h" | "--help") :: _ -> print_endline usage; exit 0
    | (("-f" | "-j" | "-C" | "-D" | "--prefix" | "--target" | "--sysroot") as o) :: [] ->
        Diag.error "%s needs a value" o
    | a :: r when String.length a > 2 && String.sub a 0 2 = "-j" ->
        let v = String.sub a 2 (String.length a - 2) in
        fl.jobs <- (try int_of_string v with _ -> Diag.error "-j wants a number, got %S" v);
        go r
    | a :: r when String.length a > 2 && String.sub a 0 2 = "-D" ->
        go ("-D" :: String.sub a 2 (String.length a - 2) :: r)
    | a :: r when String.length a > 2 && String.sub a 0 2 = "-f" ->
        fl.file <- String.sub a 2 (String.length a - 2);
        go r
    | a :: r when String.length a > 2 && String.sub a 0 2 = "-C" ->
        fl.chdir <- String.sub a 2 (String.length a - 2);
        go r
    | a :: _ when String.length a > 1 && a.[0] = '-' ->
        Diag.error ~hint:"run 'meowc --help' for the full list" "unknown option %s" a
    | a :: r -> rest := !rest @ [ a ]; go r
  in
  go argv;
  !rest

let banner p =
  if not fl.quiet then begin
    Printf.printf "%s  %s%s\n\n" (Style.bold "meowc")
      (Style.pad 40 (Style.dim fl.file))
      (Style.dim p.pname);
    flush stdout
  end

let configure () =
  if not (Sys.file_exists fl.file) then
    Diag.error ~hint:"run 'meowc init <name>' to start one" "no %s here" fl.file;
  let overrides = Hashtbl.create 8 in
  List.iter (fun (k, v) -> Hashtbl.replace overrides k v) fl.defs;
  let env = Eval.create ~overrides ~quiet:fl.quiet ~builddir:"build" ~target:fl.target in
  if fl.sysroot <> "" then
    Eval.apply_toolchain env
      [ { Ast.key = "sysroot"; kspan = Span.none;
          values = [ { Ast.text = fl.sysroot; span = Span.none; quoted = false } ] } ];
  (match fl.prefix with Some p -> Eval.setvar env "prefix" [ p ] | None -> ());
  let stmts = Parse.file fl.file in
  Eval.run env (Filename.dirname fl.file) stmts;
  env.Eval.probe.Probe.path <- Filename.concat env.Eval.tc.builddir ".meowc-probe";
  Fs.mkdir_p env.Eval.tc.builddir;
  Probe.save env.Eval.probe;
  List.iter
    (fun (k, _) ->
      if not (Hashtbl.mem env.Eval.opts k) then
        Diag.error ~hint:"only options declared with 'option' can be set" "no option named %S" k)
    fl.defs;
  let p = Resolve.project env in
  if env.Eval.probes_shown > 0 && not fl.quiet then print_newline ();
  p

let with_config_header p =
  match Configh.emit p with
  | Some (path, true) when not fl.quiet ->
      Printf.printf "  %s%s\n" (Style.yellow (Style.pad 8 "config")) path
  | _ -> ()

let unknown_target p name =
  Diag.error
    ~hint:(Suggest.hint name (List.map (fun (t : target) -> t.name) p.targets))
    "no target named %S" name

let select_targets p names =
  match names with
  | [] -> List.filter (fun (t : target) -> t.kind <> Test) p.targets
  | _ -> List.map (fun n -> match find p n with Some t -> t | None -> unknown_target p n) names

let on_start (n : Graph.node) =
  if fl.dry then Printf.printf "  %s%s\n" (Build.paint n.tag (Style.pad 8 n.tag)) n.label

let on_done ~ok ~log (n : Graph.node) =
  if ok then Printf.printf "  %s%s\n" (Build.paint n.tag (Style.pad 8 n.tag)) n.label
  else
    Printf.printf "  %s%s\n%s\n" (Style.red (Style.pad 8 "failed")) n.label
      (Style.dim ("      " ^ Exec.brief n.cmd));
  if String.trim log <> "" then (print_string log; flush stdout);
  flush stdout

let execute p ~names =
  let b = Build.of_project p in
  let targets = select_targets p names in
  let selected = Build.select b targets in
  let jobs = if fl.jobs > 0 then fl.jobs else cores () in
  if fl.dry then begin
    let order = Graph.topo b.g in
    let n = ref 0 in
    List.iter
      (fun i ->
        if Hashtbl.mem selected i then begin
          let v = b.g.Graph.nodes.(i) in
          incr n;
          Printf.printf "  %s%s\n" (Build.paint v.tag (Style.pad 8 v.tag)) v.label;
          if fl.verbose then print_endline ("      " ^ Style.dim (Exec.show v.cmd))
        end)
      order;
    Printf.printf "\n  %s\n" (Style.dim (Printf.sprintf "%d actions, nothing run" !n));
    (b, { Sched.built = 0; cached = 0; failed = 0; aborted = 0; interrupted = false })
  end
  else begin
    let cache = Cache.load (Filename.concat p.tc.builddir ".meowc-cache") in
    ignore (Graph.topo b.g);
    let r =
      Fun.protect
        ~finally:(fun () -> Cache.save cache)
        (fun () ->
          Sched.run b.g ~selected ~jobs ~cache ~verbose:fl.verbose ~keep_going:fl.keep ~on_start
            ~on_done)
    in
    (b, r)
  end

let summarise (r : Sched.result) elapsed =
  if not fl.quiet && not fl.dry then begin
    let text =
      if r.interrupted then
        Style.yellow "interrupted"
        ^ (if r.aborted > 0 then Style.dim (Printf.sprintf ", %d cancelled" r.aborted) else "")
      else if r.failed > 0 then
        Style.red (Style.plural r.failed "action" ^ " failed")
        ^ (if r.aborted > 0 then Style.dim (Printf.sprintf ", %d cancelled" r.aborted) else "")
      else if r.built = 0 then Style.dim "nothing to do"
      else Printf.sprintf "%d built%s" r.built (Style.dim (Printf.sprintf ", %d cached" r.cached))
    in
    Printf.printf "%s  %s%s\n"
      (if r.built + r.failed + r.aborted = 0 then "" else "\n")
      (Style.pad 48 text) (Style.dim (duration elapsed))
  end;
  if r.interrupted then exit 130;
  if r.failed > 0 then exit 1

let cmd_build names =
  let p = configure () in
  banner p;
  with_config_header p;
  let t0 = Unix.gettimeofday () in
  let _, r = execute p ~names in
  summarise r (Unix.gettimeofday () -. t0);
  p

let runnable_names p =
  List.filter_map (fun (t : target) -> if t.kind = Bin then Some t.name else None) p.targets
  @ List.map (fun (s : script) -> s.sname) p.scripts

let hand_over cmd =
  flush stdout;
  try Unix.execvp cmd.(0) cmd
  with Unix.Unix_error (e, _, _) -> Diag.error "cannot run %s: %s" cmd.(0) (Unix.error_message e)

let cmd_run = function
  | [] -> Diag.error ~hint:"try: meowc run <bin>" "run needs a name"
  | name :: args -> (
      let p = configure () in
      let build_first names =
        banner p;
        with_config_header p;
        let t0 = Unix.gettimeofday () in
        let _, r = execute p ~names in
        summarise r (Unix.gettimeofday () -. t0)
      in
      match List.find_opt (fun (s : script) -> s.sname = name) p.scripts with
      | Some s ->
          (* no names means everything, the same as a bare meowc build *)
          build_first s.suses;
          let cmd = Array.of_list (s.scmd @ args) in
          if not fl.quiet then Printf.printf "\n  %s\n" (Style.dim (Exec.show cmd));
          hand_over cmd
      | None ->
          let t =
            match find p name with
            | Some ({ kind = Bin; _ } as t) -> t
            | Some t -> Diag.error "%s is a %s, not a bin" name (kind_name t.kind)
            | None ->
                Diag.error ~hint:(Suggest.hint name (runnable_names p))
                  "no bin target or run block named %S" name
          in
          build_first [ name ];
          let exe = Build.bin_of p t in
          if not fl.quiet then Printf.printf "\n  %s\n" (Style.dim exe);
          hand_over (Array.of_list (exe :: args)))

let cmd_test filter =
  let p = configure () in
  banner p;
  with_config_header p;
  let all = Testrun.tests p in
  let chosen =
    match filter with
    | [] -> all
    | fs -> List.filter (fun (t : target) -> List.exists (fun f -> Glob.matches f t.name) fs) all
  in
  if chosen = [] then begin
    Printf.printf "  %s\n" (Style.dim "no test targets");
    exit 0
  end;
  let t0 = Unix.gettimeofday () in
  let _, r = execute p ~names:(List.map (fun (t : target) -> t.name) chosen) in
  if r.failed > 0 then summarise r (Unix.gettimeofday () -. t0);
  if r.built > 0 then print_newline ();
  let jobs = if fl.jobs > 0 then fl.jobs else cores () in
  let results = Testrun.run_all ~jobs p chosen in
  List.iter
    (fun (o : Testrun.outcome) ->
      Printf.printf "  %s%s%s\n"
        (if o.ok then Style.green (Style.pad 8 "pass") else Style.red (Style.pad 8 "fail"))
        (Style.pad 40 o.name)
        (Style.dim (Printf.sprintf "%.0f ms" o.ms));
      if not o.ok then begin
        print_string o.output;
        Printf.printf "  %s\n" (Style.dim (Printf.sprintf "exit %d" o.code))
      end)
    results;
  let passed = List.length (List.filter (fun (o : Testrun.outcome) -> o.ok) results) in
  let total = List.length results in
  let text =
    if passed = total then Style.green (Printf.sprintf "%d of %d passed" passed total)
    else Style.red (Printf.sprintf "%d of %d passed" passed total)
  in
  Printf.printf "\n  %s%s\n" (Style.pad 48 text)
    (Style.dim (duration (Unix.gettimeofday () -. t0)));
  if passed <> total then exit 1

let cmd_install () =
  let p = cmd_build [] in
  let items = Install.execute ~dry:fl.dry p in
  print_newline ();
  List.iter
    (fun (src, dst) ->
      Printf.printf "  %s%s %s %s\n"
        (Style.green (Style.pad 8 (if fl.dry then "would" else "install")))
        (Style.pad 30 (Filename.basename src)) (Style.dim "->") (Style.dim dst))
    items;
  if items = [] then Printf.printf "  %s\n" (Style.dim "nothing is marked for installation")
  else Printf.printf "\n  %s\n" (Style.dim (Style.plural (List.length items) "file" ^ " under " ^ p.prefix))

let cmd_targets () =
  let p = configure () in
  banner p;
  List.iter
    (fun (t : target) ->
      Printf.printf "  %s%s%s%s\n"
        (Build.paint (Build.tag_of t.kind) (Style.pad 9 (kind_name t.kind)))
        (Style.pad 22 t.name)
        (Style.pad 20 (Style.dim (Style.plural (List.length (all_srcs t)) "source")))
        (Style.dim (if t.uses = [] then "" else "uses " ^ String.concat " " t.uses)))
    p.targets;
  List.iter
    (fun (s : script) ->
      Printf.printf "  %s%s%s\n"
        (Style.yellow (Style.pad 9 "run"))
        (Style.pad 22 s.sname)
        (Style.dim (if s.suses = [] then "" else "uses " ^ String.concat " " s.suses)))
    p.scripts;
  Printf.printf "\n  %s\n"
    (Style.dim
       (String.concat ", "
          (Style.plural (List.length p.targets) "target"
          :: (if p.scripts = [] then [] else [ Style.plural (List.length p.scripts) "run block" ]))))

(* Graphviz draws vectors, and a browser is the one viewer everyone has that
   renders them without turning the text into pixels. *)
let browsers = [ "firefox"; "xdg-open" ]

let graphical () =
  Sys.getenv_opt "DISPLAY" <> None || Sys.getenv_opt "WAYLAND_DISPLAY" <> None

let words v = List.filter (fun w -> w <> "") (String.split_on_char ' ' v)

(* A browser reads a relative path as something to search for, not a file *)
let file_url p =
  "file://" ^ if Filename.is_relative p then Filename.concat (Sys.getcwd ()) p else p

let viewer svg =
  let named =
    match (Sys.getenv_opt "MEOWC_VIEWER", Sys.getenv_opt "BROWSER") with
    | Some v, _ when String.trim v <> "" -> Some (words v)
    | _, Some v when String.trim v <> "" -> Some (words v)
    | _ -> Option.map (fun b -> [ b ]) (List.find_opt Exec.which browsers)
  in
  Option.map (fun argv -> Array.of_list (argv @ [ file_url svg ])) named

(* The viewer outlives meowc, and its startup chatter is not meowc's output *)
let spawn_detached cmd =
  flush stdout;
  let nul = Exec.devnull () in
  let quiet = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  (match Unix.create_process cmd.(0) cmd nul quiet quiet with
  | _ -> ()
  | exception Unix.Unix_error (e, _, _) ->
      Diag.error "cannot run %s: %s" cmd.(0) (Unix.error_message e));
  Unix.close nul;
  Unix.close quiet

let inline () =
  Sys.getenv_opt "KITTY_WINDOW_ID" <> None && Unix.isatty Unix.stdout && Exec.which "kitten"

let draw ~svg dot =
  if not (Exec.which "dot") then
    Diag.error ~hint:"install graphviz" "dot is not on the path, so there is nothing to draw with";
  let src = Filename.temp_file "meowc-graph" ".dot" in
  Fs.mkdir_p (Filename.dirname svg);
  Fs.write src dot;
  let rendered = Exec.ok [| "dot"; "-Tsvg"; "-o"; svg; src |] in
  let finish () = try Sys.remove src with Sys_error _ -> () in
  if not rendered then (finish (); Diag.error "dot could not render the graph");
  (match (if graphical () then viewer svg else None) with
  | Some cmd -> spawn_detached cmd
  | None ->
      if inline () then begin
        (* a terminal cannot draw vectors, so this one branch wants pixels *)
        let png = Filename.remove_extension svg ^ ".png" in
        ignore (Exec.ok [| "dot"; "-Tpng"; "-o"; png; src |]);
        flush stdout;
        let cmd = [| "kitten"; "icat"; png |] in
        match Unix.create_process cmd.(0) cmd Unix.stdin Unix.stdout Unix.stderr with
        | pid -> ignore (Unix.waitpid [] pid)
        | exception Unix.Unix_error (e, _, _) ->
            Diag.error "cannot run kitten: %s" (Unix.error_message e)
      end
      else Printf.printf "  %s %s\n" (Style.dim "wrote") svg);
  finish ()

(* Raw graphviz on a terminal is for nobody, so draw it there instead *)
let drawing () = fl.show || (fl.dot && Unix.isatty Unix.stdout)

let cmd_graph names =
  let p = configure () in
  let svg = Filename.concat p.tc.builddir "graph.svg" in
  if fl.targets_only then
    let text = Dot.targets_only p in
    if drawing () then draw ~svg text else print_string text
  else
    let b = Build.of_project p in
    let selected =
      match names with [] -> None | _ -> Some (Build.select b (select_targets p names))
    in
    let keep i = match selected with None -> true | Some s -> Hashtbl.mem s i in
    if drawing () then draw ~svg (Dot.render ?selected b)
    else if fl.dot then print_string (Dot.render ?selected b)
    else begin
      banner p;
      let order = Graph.topo b.g in
      let shown = ref 0 in
      List.iter
        (fun i ->
          if keep i then begin
          incr shown;
          let v = b.g.Graph.nodes.(i) in
          Printf.printf "  %s%s\n" (Build.paint v.tag (Style.pad 8 v.tag)) (Style.pad 34 v.label);
          List.iter
            (fun d -> Printf.printf "      %s %s\n" (Style.dim "<-") (Style.dim b.g.Graph.nodes.(d).label))
            v.deps
          end)
        order;
      Printf.printf "\n  %s\n"
        (Style.dim (Style.plural !shown "action"))
    end

let cmd_explain = function
  | [] -> Diag.error "explain needs a file path"
  | name :: _ ->
      let p = configure () in
      let b = Build.of_project p in
      banner p;
      (* a target name stands for whatever it produces *)
      let path = match find p name with Some t -> Build.out_of p t | None -> name in
      (match Graph.producing b.g path with
      | None ->
          Printf.printf "  %s is not produced by this build\n" (Style.bold path);
          let users =
            Array.to_list b.g.Graph.nodes
            |> List.filter (fun (n : Graph.node) -> List.mem path (Sched.inputs_of n))
          in
          if users <> [] then begin
            Printf.printf "  %s\n" (Style.dim "it is an input to:");
            List.iter (fun (n : Graph.node) -> Printf.printf "      %s %s\n" (Build.paint n.tag n.tag) n.label) users
          end
      | Some i ->
          let n = b.g.Graph.nodes.(i) in
          let cache = Cache.load (Filename.concat p.tc.builddir ".meowc-cache") in
          let fresh = Sched.up_to_date cache n in
          Printf.printf "  %s%s\n" (Build.paint n.tag (Style.pad 8 n.tag)) (Style.bold n.label);
          Printf.printf "      %s %s\n" (Style.dim "command") (Style.dim (Exec.show n.cmd));
          Printf.printf "      %s %s\n" (Style.dim "state  ")
            (if fresh then Style.green "up to date" else Style.yellow "will rebuild");
          if not fresh then begin
            if not (List.for_all Sys.file_exists n.outs) then
              Printf.printf "      %s %s\n" (Style.dim "reason ") "output is missing"
            else if Cache.current cache (List.hd n.outs) = None then
              Printf.printf "      %s %s\n" (Style.dim "reason ") "no cached key for this output"
            else Printf.printf "      %s %s\n" (Style.dim "reason ") "an input or the command changed"
          end;
          Printf.printf "      %s\n" (Style.dim "inputs");
          List.iter
            (fun i ->
              let d = match Cache.digest_file i with Some d -> String.sub d 0 8 | None -> "missing " in
              Printf.printf "        %s  %s\n" (Style.dim d) i)
            (Sched.inputs_of n))

let cmd_compdb () =
  let p = configure () in
  let b = Build.of_project p in
  let path = "compile_commands.json" in
  let n = Compdb.write b path in
  Printf.printf "  %s%s %s\n" (Style.green (Style.pad 8 "wrote")) path
    (Style.dim (Style.plural n "entry"))

let cmd_watch names =
  let round () =
    let p = configure () in
    with_config_header p;
    let t0 = Unix.gettimeofday () in
    let b, r = execute p ~names in
    summarise r (Unix.gettimeofday () -. t0);
    b
  in
  let rec loop () =
    let b = (try Some (round ()) with Diag.Stop ds -> Diag.print ds; None) in
    let files = match b with Some b -> Watch.inputs b | None -> [ fl.file ] in
    let files = if List.mem fl.file files then files else fl.file :: files in
    Printf.printf "\n  %s %s\n" (Style.dim "watching") (Style.dim (Style.plural (List.length files) "file"));
    flush stdout;
    let changed = Watch.wait ~interval:fl.interval files in
    Printf.printf "\n  %s %s\n\n" (Style.yellow "changed") (String.concat " " (List.map Filename.basename changed));
    flush stdout;
    loop ()
  in
  loop ()

let cmd_clean () =
  let p = configure () in
  if Sys.file_exists p.tc.builddir then begin
    Fs.rm_rf p.tc.builddir;
    Printf.printf "  %s%s\n" (Style.dim (Style.pad 8 "removed")) p.tc.builddir
  end
  else Printf.printf "  %s\n" (Style.dim "already clean")

let cmd_init = function
  | [] -> Diag.error ~hint:"try: meowc init hello" "init needs a project name"
  | name :: _ ->
      let files = Scaffold.create name in
      Printf.printf "  %s %s\n\n" (Style.bold "meowc") (Style.dim ("new project " ^ name));
      List.iter (fun f -> Printf.printf "  %s%s\n" (Style.green (Style.pad 8 "new")) f) files;
      Printf.printf "\n  %s\n" (Style.dim "now run: meowc run " ^ name)

let cmd_check () =
  let p = configure () in
  banner p;
  let b = Build.of_project p in
  ignore (Graph.topo b.g);
  let srcs = List.length (List.concat_map all_srcs p.targets) in
  Printf.printf "  %s\n"
    (String.concat (Style.dim ", ")
       [ Style.plural (List.length p.targets) "target"; Style.plural srcs "source";
         Style.plural (List.length p.rules) "generated file";
         Style.plural (Array.length b.g.Graph.nodes) "action" ])

let () =
  (try
     let rest = parse_args (List.tl (Array.to_list Sys.argv)) in
     if not (Unix.isatty Unix.stdout) then Style.enabled := false;
     if fl.chdir <> "" then Sys.chdir fl.chdir;
     match rest with
     | "build" :: names -> ignore (cmd_build names)
     | "run" :: rest -> cmd_run rest
     | "test" :: f -> cmd_test f
     | "install" :: _ -> cmd_install ()
     | "configure" :: _ ->
         let p = configure () in
         banner p;
         with_config_header p;
         Printf.printf "  %s\n" (Style.dim "configured")
     | "check" :: _ -> cmd_check ()
     | "targets" :: _ -> cmd_targets ()
     | "graph" :: names -> cmd_graph names
     | "explain" :: rest -> cmd_explain rest
     | "compdb" :: _ -> cmd_compdb ()
     | "watch" :: names -> cmd_watch names
     | "init" :: rest -> cmd_init rest
     | "clean" :: _ -> cmd_clean ()
     | "help" :: _ -> print_endline usage
     | names -> ignore (cmd_build names)
   with
  | Diag.Stop ds -> Diag.print ds; exit 1
  | Sys_error msg -> prerr_endline ("  " ^ Style.red "error" ^ " " ^ msg); exit 1
  | Unix.Unix_error (e, f, a) ->
      prerr_endline ("  " ^ Style.red "error" ^ Printf.sprintf " %s: %s %s" f (Unix.error_message e) a);
      exit 1)
