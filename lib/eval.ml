open Ast

type opt = { oname : string; oty : string; odefault : string list; ospan : Span.t }

type env = {
  vars : (string, string list) Hashtbl.t;
  mutable declared : string list;
  opts : (string, opt) Hashtbl.t;
  overrides : (string, string) Hashtbl.t;
  probe : Probe.t;
  mutable tc : Types.toolchain;
  mutable blocks : (Ast.block * string) list;
  mutable defines : Types.define list;
  mutable config_header : string option;
  pkgs : (string, string list * string list) Hashtbl.t;
  mutable stack : string list;
  mutable pname : string;
  mutable quiet : bool;
  mutable probes_shown : int;
}

let placeholders =
  [ "in"; "out"; "stem"; "dir"; "base"; "ext"; "name" ]
  @ List.init 8 (fun i -> Printf.sprintf "out%d" i)
  @ List.init 8 (fun i -> Printf.sprintf "in%d" i)

let truthy = function
  | [] -> false
  | l -> (
      match String.lowercase_ascii (String.concat " " l) with
      | "" | "0" | "false" | "no" | "off" | "n" -> false
      | _ -> true)

let setvar env name values =
  if not (Hashtbl.mem env.vars name) then env.declared <- env.declared @ [ name ];
  Hashtbl.replace env.vars name values

let var_names env = List.sort compare env.declared

let uname () =
  let code, out = Exec.capture [| "uname"; "-s" |] in
  if code = 0 then String.lowercase_ascii (String.trim out) else "unknown"

let arch () =
  let code, out = Exec.capture [| "uname"; "-m" |] in
  if code = 0 then String.trim out else "unknown"

let compiler_id cc =
  let code, out = Exec.capture [| cc; "--version" |] in
  if code <> 0 then "unknown"
  else
    let l = String.lowercase_ascii out in
    let has s =
      let n = String.length s and m = String.length l in
      let rec go i = i + n <= m && (String.sub l i n = s || go (i + 1)) in
      go 0
    in
    if has "clang" then "clang"
    else if has "free software foundation" || has "gcc" then "gcc"
    else if has "tcc" then "tcc"
    else "unknown"

let triple_arch t = match String.index_opt t '-' with Some i -> String.sub t 0 i | None -> t

let triple_platform t =
  let has sub =
    let n = String.length sub and m = String.length t in
    let rec go i = i + n <= m && (String.sub t i n = sub || go (i + 1)) in
    go 0
  in
  if has "linux" then "linux"
  else if has "darwin" || has "apple" || has "macos" then "darwin"
  else if has "mingw" || has "windows" || has "cygwin" then "windows"
  else if has "freebsd" || has "openbsd" || has "netbsd" || has "dragonfly" then "bsd"
  else if has "wasi" || has "emscripten" then "wasm"
  else if has "none" || has "eabi" then "bare"
  else "unknown"

let default_tc : Types.toolchain =
  {
    cc = "cc";
    cxx = "c++";
    ar = "ar";
    ranlib = "ranlib";
    target = "";
    sysroot = "";
    xflags = [];
    cflags = [];
    cxxflags = [];
    ldflags = [];
    builddir = "build";
  }

let platform_vars env plat arch =
  List.iter (fun (k, v) -> setvar env k [ v ])
    [
      ("platform", plat);
      ("arch", arch);
      ("linux", if plat = "linux" then "true" else "false");
      ("darwin", if plat = "darwin" then "true" else "false");
      ("bsd", if plat = "bsd" then "true" else "false");
      ("windows", if plat = "windows" then "true" else "false");
      ("unix", if plat = "windows" || plat = "bare" || plat = "wasm" then "false" else "true");
    ]

let derive_cross env explicit =
  let tc = env.tc in
  let t = tc.target in
  let set k = Hashtbl.mem explicit k in
  let prefixed n = t ^ "-" ^ n in
  let have_prefixed = Exec.which (prefixed "gcc") || Exec.which (prefixed "cc") in
  let tc =
    if not have_prefixed then tc
    else
      let pick n alt = if Exec.which (prefixed n) then prefixed n else prefixed alt in
      {
        tc with
        cc = (if set "cc" then tc.cc else pick "gcc" "cc");
        cxx = (if set "cxx" then tc.cxx else pick "g++" "c++");
        ar = (if set "ar" then tc.ar else prefixed "ar");
        ranlib = (if set "ranlib" then tc.ranlib else prefixed "ranlib");
      }
  in
  let xflags =
    (if have_prefixed then [] else [ "--target=" ^ t ])
    @ (if tc.sysroot = "" then [] else [ "--sysroot=" ^ tc.sysroot ])
  in
  let tc = { tc with xflags } in
  let tc =
    if set "builddir" || Filename.basename tc.builddir = t then tc
    else { tc with builddir = Filename.concat tc.builddir t }
  in
  env.tc <- tc;
  platform_vars env (triple_platform t) (triple_arch t)

let create ~overrides ~quiet ~builddir ~target =
  let builddir = if target = "" then builddir else Filename.concat builddir target in
  let tc = { default_tc with builddir; target } in
  let env =
    {
      vars = Hashtbl.create 64;
      declared = [];
      opts = Hashtbl.create 16;
      overrides;
      probe = Probe.load (Filename.concat builddir ".meowc-probe");
      tc;
      blocks = [];
      defines = [];
      config_header = None;
      pkgs = Hashtbl.create 8;
      stack = [];
      pname = "unnamed";
      quiet;
      probes_shown = 0;
    }
  in
  let host_plat =
    match uname () with
    | "linux" -> "linux"
    | "darwin" -> "darwin"
    | "freebsd" | "openbsd" | "netbsd" -> "bsd"
    | s -> s
  in
  if target = "" then platform_vars env host_plat (arch ())
  else platform_vars env (triple_platform target) (triple_arch target);
  List.iter (fun (k, v) -> setvar env k [ v ])
    [
      ("host", host_plat);
      ("target", target);
      ("sysroot", "");
      ("cross", if target = "" then "false" else "true");
      ("builddir", builddir);
      ("cc", tc.cc);
      ("cxx", tc.cxx);
      ("cc_id", "unknown");
      ("project", env.pname);
      ("prefix", "/usr/local");
    ];
  if target <> "" then derive_cross env (Hashtbl.create 1);
  env

let lookup env name span =
  if String.length name > 4 && String.sub name 0 4 = "env." then
    let k = String.sub name 4 (String.length name - 4) in
    [ (match Sys.getenv_opt k with Some v -> v | None -> "") ]
  else
    match Hashtbl.find_opt env.vars name with
    | Some l -> l
    | None ->
        if List.mem name placeholders then [ "${" ^ name ^ "}" ]
        else
          Diag.error ~span ~hint:(Suggest.hint name (var_names env)) "unknown variable ${%s}" name

let is_whole_ref t =
  let n = String.length t in
  n > 3
  && String.sub t 0 2 = "${"
  && t.[n - 1] = '}'
  && match String.index_opt t '}' with Some i -> i = n - 1 | None -> false

let expand env (v : value) =
  let t = v.text in
  let n = String.length t in
  if is_whole_ref t then lookup env (String.sub t 2 (n - 3)) v.span
  else begin
    let b = Buffer.create n in
    let i = ref 0 in
    while !i < n do
      if t.[!i] = '$' && !i + 1 < n && t.[!i + 1] = '{' then
        match String.index_from_opt t (!i + 2) '}' with
        | None -> (Buffer.add_char b t.[!i]; incr i)
        | Some j ->
            let name = String.sub t (!i + 2) (j - !i - 2) in
            Buffer.add_string b (String.concat " " (lookup env name v.span));
            i := j + 1
      else (Buffer.add_char b t.[!i]; incr i)
    done;
    [ Buffer.contents b ]
  end

let expand_all env vs = List.concat_map (expand env) vs
let expand1 env v = String.concat " " (expand env v)

let rec eval_expr env = function
  | Atom v ->
      if v.quoted || String.length v.text > 1 && v.text.[0] = '$' then truthy (expand env v)
      else truthy (lookup env v.text v.span)
  | Not (e, _) -> not (eval_expr env e)
  | And (a, b) -> eval_expr env a && eval_expr env b
  | Or (a, b) -> eval_expr env a || eval_expr env b
  | Cmp (a, op, b) ->
      let x = expand1 env a and y = expand1 env b in
      if op = "==" then x = y else x <> y

let show_probe env label result detail =
  if not env.quiet then begin
    env.probes_shown <- env.probes_shown + 1;
    Printf.printf "  %s%s%s\n"
      (Style.blue (Style.pad 8 "check"))
      (Style.pad 46 label)
      (if result then Style.green detail else Style.dim detail);
    flush stdout
  end

let define env name value = env.defines <- env.defines @ [ { Types.dname = name; dvalue = value } ]

let record env ~label ~var ~macro (o : Probe.outcome) ~detail =
  setvar env var [ (if o.ok then "true" else "false") ];
  define env macro (if o.ok then Some "1" else None);
  if env.probe.Probe.ran > 0 then show_probe env label o.ok detail

let run_check env (c : check) =
  let before = env.probe.Probe.ran in
  let fresh () = env.probe.Probe.ran > before in
  match c with
  | Header h ->
      let name = expand1 env h in
      let o = Probe.header env.probe name in
      setvar env (Ident.have_var name) [ (if o.ok then "true" else "false") ];
      define env (Ident.have name) (if o.ok then Some "1" else None);
      if fresh () then show_probe env ("header " ^ name) o.ok (if o.ok then "yes" else "no")
  | Func (f, lib) ->
      let name = expand1 env f in
      let l = Option.map (expand1 env) lib in
      let o = Probe.func env.probe name l in
      setvar env (Ident.have_var name) [ (if o.ok then "true" else "false") ];
      define env (Ident.have name) (if o.ok then Some "1" else None);
      let label = "func " ^ name ^ (match l with Some x -> " in " ^ x | None -> "") in
      if fresh () then show_probe env label o.ok (if o.ok then "yes" else "no")
  | Symbol (s, h) ->
      let name = expand1 env s and hdr = expand1 env h in
      let o = Probe.symbol env.probe name hdr in
      setvar env (Ident.have_var name) [ (if o.ok then "true" else "false") ];
      define env (Ident.have name) (if o.ok then Some "1" else None);
      if fresh () then
        show_probe env ("symbol " ^ name ^ " in " ^ hdr) o.ok (if o.ok then "yes" else "no")
  | Sizeof ty ->
      let name = expand1 env ty in
      let o = Probe.sizeof env.probe name in
      setvar env (Ident.sizeof_var name) [ (if o.ok then o.value else "0") ];
      define env (Ident.sizeof_name name) (if o.ok then Some o.value else None);
      if fresh () then
        show_probe env ("sizeof " ^ name) o.ok (if o.ok then o.value else "unknown")
  | Compiles (n, body) ->
      let name = expand1 env n in
      let o = Probe.snippet env.probe name (expand1 env body) in
      setvar env (Ident.have_var name) [ (if o.ok then "true" else "false") ];
      define env (Ident.have name) (if o.ok then Some "1" else None);
      if fresh () then show_probe env ("compiles " ^ name) o.ok (if o.ok then "yes" else "no")

let run_pkg env name cons span =
  let n = expand1 env name in
  let c = Option.map (expand1 env) cons in
  let before = env.probe.Probe.ran in
  let o = Probe.pkg_config env.probe n c in
  let cflags, libs = Probe.pkg_parts o in
  Hashtbl.replace env.pkgs n (cflags, libs);
  setvar env (Ident.have_var n) [ (if o.ok then "true" else "false") ];
  setvar env (n ^ "_cflags") cflags;
  setvar env (n ^ "_libs") libs;
  define env (Ident.have n) (if o.ok then Some "1" else None);
  ignore span;
  if env.probe.Probe.ran > before then
    show_probe env
      ("pkg " ^ n ^ (match c with Some x -> " " ^ x | None -> ""))
      o.ok
      (if o.ok then if o.note = "" then "yes" else o.note else "not found")

let under base p =
  if base = "" || base = "." then p
  else if String.length p > 0 && p.[0] = '/' then p
  else Filename.concat base p

let rec flatten_items env items =
  List.concat_map
    (function
      | FField f -> [ f ]
      | FIf (arms, els) -> (
          match List.find_opt (fun (c, _) -> eval_expr env c) arms with
          | Some (_, body) -> flatten_items env body
          | None -> flatten_items env els))
    items

let apply_toolchain env fields =
  let explicit = Hashtbl.create 8 in
  List.iter (fun (f : field) -> Hashtbl.replace explicit f.key ()) fields;
  List.iter
    (fun (f : field) ->
      let vs = expand_all env f.values in
      let one () = String.concat " " vs in
      let tc = env.tc in
      env.tc <-
        (match f.key with
        | "cc" -> { tc with cc = one () }
        | "cxx" -> { tc with cxx = one () }
        | "ar" -> { tc with ar = one () }
        | "ranlib" -> { tc with ranlib = one () }
        | "cflags" -> { tc with cflags = tc.cflags @ vs }
        | "cxxflags" -> { tc with cxxflags = tc.cxxflags @ vs }
        | "ldflags" -> { tc with ldflags = tc.ldflags @ vs }
        | "builddir" -> { tc with builddir = one () }
        | "target" -> { tc with target = one () }
        | "sysroot" -> { tc with sysroot = one () }
        | k ->
            Diag.error ~span:f.kspan
              ~hint:
                (Suggest.hint k
                   [ "cc"; "cxx"; "ar"; "ranlib"; "cflags"; "cxxflags"; "ldflags"; "builddir";
                     "target"; "sysroot" ])
              "toolchain has no field %S" k))
    fields;
  if env.tc.target <> "" then derive_cross env explicit;
  setvar env "cc" [ env.tc.cc ];
  setvar env "cxx" [ env.tc.cxx ];
  setvar env "target" [ env.tc.target ];
  setvar env "sysroot" [ env.tc.sysroot ];
  setvar env "cross" [ (if env.tc.target = "" then "false" else "true") ];
  setvar env "builddir" [ env.tc.builddir ];
  setvar env "cc_id" [ compiler_id env.tc.cc ];
  env.probe.Probe.cc <- env.tc.cc;
  env.probe.Probe.cflags <- env.tc.xflags @ env.tc.cflags;
  env.probe.Probe.ldflags <- env.tc.xflags @ env.tc.ldflags

let rec predeclare env stmts =
  let name (v : value) = v.text in
  List.iter
    (fun st ->
      match st with
      | Check (c, _) -> (
          match c with
          | Header h -> setvar env (Ident.have_var (name h)) [ "false" ]
          | Func (f, _) -> setvar env (Ident.have_var (name f)) [ "false" ]
          | Symbol (s, _) -> setvar env (Ident.have_var (name s)) [ "false" ]
          | Sizeof t -> setvar env (Ident.sizeof_var (name t)) [ "0" ]
          | Compiles (n, _) -> setvar env (Ident.have_var (name n)) [ "false" ])
      | Pkg (n, _, _) ->
          setvar env (Ident.have_var (name n)) [ "false" ];
          setvar env (name n ^ "_cflags") [];
          setvar env (name n ^ "_libs") []
      | Option (n, _, d) ->
          if not (Hashtbl.mem env.vars n.text) then
            setvar env n.text
              (match Hashtbl.find_opt env.overrides n.text with
              | Some v -> String.split_on_char ' ' v |> List.filter (fun s -> s <> "")
              | None -> List.map name d)
      | If (arms, els) ->
          List.iter (fun (_, b) -> predeclare env b) arms;
          predeclare env els
      | _ -> ())
    stmts

let rec run env base stmts =
  predeclare env stmts;
  List.iter (run_stmt env base) stmts

and run_stmt env base st =
  match st with
  | Project n ->
      env.pname <- expand1 env n;
      setvar env "project" [ env.pname ]
  | Set (n, vs) -> setvar env n.text (expand_all env vs)
  | Append (n, vs) ->
      let prev = match Hashtbl.find_opt env.vars n.text with Some l -> l | None -> [] in
      setvar env n.text (prev @ expand_all env vs)
  | Option (n, ty, dflt) ->
      let o = { oname = n.text; oty = ty.text; odefault = expand_all env dflt; ospan = n.span } in
      Hashtbl.replace env.opts n.text o;
      let value =
        match Hashtbl.find_opt env.overrides n.text with
        | Some v -> String.split_on_char ' ' v |> List.filter (fun s -> s <> "")
        | None -> o.odefault
      in
      setvar env n.text value
  | Include v ->
      let path = under base (expand1 env v) in
      splice env (Filename.dirname path) path v.span
  | Subdir v ->
      let dir = under base (expand1 env v) in
      let path = Filename.concat dir "build.meow" in
      splice env dir path v.span
  | If (arms, els) -> (
      match List.find_opt (fun (c, _) -> eval_expr env c) arms with
      | Some (_, body) -> run env base body
      | None -> run env base els)
  | Check (c, _) -> run_check env c
  | Pkg (n, c, span) -> run_pkg env n c span
  | ConfigHeader v -> env.config_header <- Some (under base (expand1 env v))
  | Message (lvl, vs) ->
      let text = String.concat " " (expand_all env vs) in
      (match lvl.text with
      | "error" -> Diag.error ~span:lvl.span "%s" text
      | "warn" -> Printf.eprintf "  %s %s\n" (Style.yellow "warning") text
      | _ -> if not env.quiet then Printf.printf "  %s %s\n" (Style.blue "note") text)
  | Blk b ->
      let fields = flatten_items env b.items in
      if b.kind = "toolchain" then apply_toolchain env fields
      else
        let expanded =
          List.map
            (fun (f : field) ->
              {
                f with
                values =
                  List.concat_map
                    (fun (v : value) ->
                      List.map (fun t -> { text = t; span = v.span; quoted = v.quoted }) (expand env v))
                    f.values;
              })
            fields
        in
        env.blocks <-
          env.blocks @ [ ({ b with items = List.map (fun f -> FField f) expanded }, base) ]

and splice env dir path span =
  if List.mem path env.stack then
    Diag.error ~span "include cycle: %s" (String.concat " -> " (List.rev (path :: env.stack)));
  if not (Sys.file_exists path) then Diag.error ~span "cannot read %s" path;
  env.stack <- path :: env.stack;
  run env dir (Parse.file path);
  env.stack <- List.tl env.stack
