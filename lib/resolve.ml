open Ast
open Types

let target_fields =
  [ "srcs"; "include"; "use"; "cflags"; "cxxflags"; "ldflags"; "define"; "pkg"; "install";
    "soname"; "args" ]

let rule_fields = [ "inputs"; "outputs"; "command"; "description" ]
let install_fields = [ "files"; "to" ]

let fields_of (b : block) =
  List.filter_map (function FField f -> Some f | FIf _ -> None) b.items

let check_fields (b : block) known =
  List.iter
    (fun (f : field) ->
      if not (List.mem f.key known) then
        Diag.error ~span:f.kspan ~hint:(Suggest.hint f.key known) "%s %s has no field %S" b.kind
          b.bname f.key)
    (fields_of b)

let get b key = List.concat_map (fun (f : field) -> if f.key = key then f.values else []) (fields_of b)
let texts vs = List.map (fun (v : value) -> v.text) vs
let get1 b key = match texts (get b key) with [] -> None | l -> Some (String.concat " " l)

let subst table s =
  let n = String.length s in
  let b = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    if s.[!i] = '$' && !i + 1 < n && s.[!i + 1] = '{' then
      match String.index_from_opt s (!i + 2) '}' with
      | None -> (Buffer.add_char b s.[!i]; incr i)
      | Some j ->
          let name = String.sub s (!i + 2) (j - !i - 2) in
          (match List.assoc_opt name table with
          | Some v -> Buffer.add_string b v
          | None -> Buffer.add_string b (String.sub s !i (j - !i + 1)));
          i := j + 1
    else (Buffer.add_char b s.[!i]; incr i)
  done;
  Buffer.contents b

let rules_of blocks =
  List.concat_map
    (fun ((b : block), base) ->
      if b.kind <> "rule" then []
      else begin
        check_fields b rule_fields;
        let inputs = texts (get b "inputs") in
        let outputs = texts (get b "outputs") in
        let command = texts (get b "command") in
        let descr = Option.value (get1 b "description") ~default:("gen " ^ b.bname) in
        if outputs = [] then Diag.error ~span:b.nspan "rule %s declares no outputs" b.bname;
        if command = [] then Diag.error ~span:b.nspan "rule %s declares no command" b.bname;
        let files =
          List.concat_map (fun p -> Glob.expand (Eval.under base p)) inputs |> List.sort_uniq compare
        in
        if files = [] then
          Diag.error ~span:b.nspan ~hint:"the inputs pattern matched nothing"
            "rule %s has no inputs" b.bname;
        List.map
          (fun f ->
            let stem = Filename.remove_extension (Filename.basename f) in
            let table0 =
              [ ("in", f); ("stem", stem); ("dir", Filename.dirname f);
                ("base", Filename.basename f); ("ext", Filename.extension f); ("name", b.bname) ]
            in
            let routs = List.map (fun o -> Eval.under base (subst table0 o)) outputs in
            let table =
              table0 @ ("out", List.hd routs)
              :: List.mapi (fun i o -> (Printf.sprintf "out%d" i, o)) routs
            in
            {
              rname = b.bname;
              rin = [ f ];
              routs;
              rcmd = List.map (subst table) command;
              rdesc = subst table descr;
              rspan = b.nspan;
            })
          files
      end)
    blocks

let target_of pkgs ((b : block), base) =
  check_fields b target_fields;
  let kind =
    match b.kind with
    | "lib" -> Lib
    | "shared" -> Shared
    | "bin" -> Bin
    | _ -> Test
  in
  let pkg_names = texts (get b "pkg") in
  let pkg_cflags = List.concat_map (fun p ->
      match Hashtbl.find_opt pkgs p with
      | Some (c, _) -> c
      | None -> Diag.error ~span:b.nspan
          ~hint:(Printf.sprintf "add 'pkg %s' at the top level first" p)
          "%s uses pkg %S, which was never checked" b.bname p) pkg_names
  in
  let pkg_libs = List.concat_map (fun p ->
      match Hashtbl.find_opt pkgs p with Some (_, l) -> l | None -> []) pkg_names
  in
  {
    name = b.bname;
    kind;
    dir = base;
    srcs = [];
    gen_srcs = [];
    includes = List.map (Eval.under base) (texts (get b "include"));
    uses = texts (get b "use");
    cflags = texts (get b "cflags") @ pkg_cflags;
    cxxflags = texts (get b "cxxflags") @ pkg_cflags;
    ldflags = texts (get b "ldflags") @ pkg_libs;
    defines = texts (get b "define");
    pkgs = pkg_names;
    install = get1 b "install";
    soname = get1 b "soname";
    args = texts (get b "args");
    span = b.nspan;
  }

let sources_of ~extra ((b : block), base) (t : target) =
  let pats = texts (get b "srcs") in
  if pats = [] then
    Diag.error ~span:b.nspan ~hint:"add a line like: srcs src/*.c" "%s %s declares no srcs"
      b.kind b.bname;
  let files =
    List.concat_map (fun p -> Glob.expand_with ~extra (Eval.under base p)) pats
    |> List.sort_uniq compare
  in
  if files = [] then
    Diag.error ~span:b.nspan
      ~hint:(Printf.sprintf "no file matches %s" (String.concat " " pats))
      "%s %s matched no sources" b.kind b.bname;
  let gen, real = List.partition (fun f -> List.mem f extra) files in
  { t with srcs = real; gen_srcs = gen }

let installs_of blocks =
  List.concat_map
    (fun ((b : block), base) ->
      if b.kind <> "install" then []
      else begin
        check_fields b install_fields;
        let dest = match get1 b "to" with
          | Some d -> d
          | None -> Diag.error ~span:b.nspan ~hint:"add: to include" "install %s has no destination" b.bname
        in
        let files =
          List.concat_map (fun p -> Glob.expand (Eval.under base p)) (texts (get b "files"))
        in
        if files = [] then Diag.error ~span:b.nspan "install %s matched no files" b.bname;
        List.map (fun f -> { from = f; dest }) files
      end)
    blocks

let project (env : Eval.env) =
  let blocks = env.blocks in
  let rules = rules_of blocks in
  let generated = List.concat_map (fun r -> r.routs) rules in
  let tblocks =
    List.filter (fun ((b : block), _) -> List.mem b.kind [ "lib"; "shared"; "bin"; "test" ]) blocks
  in
  let targets =
    List.map (fun bb -> sources_of ~extra:generated bb (target_of env.pkgs bb)) tblocks
  in
  List.iter
    (fun (b : block) ->
      if not (List.mem b.kind [ "lib"; "shared"; "bin"; "test"; "rule"; "install"; "toolchain" ]) then
        Diag.error ~span:b.kspan "unknown block %S" b.kind)
    (List.map fst blocks);
  let seen = Hashtbl.create 8 in
  List.iter
    (fun t ->
      match Hashtbl.find_opt seen t.name with
      | Some (prev : target) ->
          Diag.error ~span:t.span ~notes:[ (prev.span, "first declared here") ]
            "target %S is declared twice" t.name
      | None -> Hashtbl.add seen t.name t)
    targets;
  let p =
    {
      pname = env.pname;
      tc = env.tc;
      targets;
      rules;
      defines = env.defines;
      config_header = env.config_header;
      installs = installs_of blocks;
      prefix = (match Hashtbl.find_opt env.vars "prefix" with Some [ p ] -> p | _ -> "/usr/local");
      platform = (match Hashtbl.find_opt env.vars "platform" with Some [ p ] -> p | _ -> "linux");
    }
  in
  List.iter
    (fun t ->
      List.iter
        (fun u ->
          match find p u with
          | None ->
              Diag.error ~span:t.span
                ~hint:(Suggest.hint u (List.map (fun x -> x.name) targets))
                "%s uses %S, which is not a target" t.name u
          | Some x when x.kind = Bin || x.kind = Test ->
              Diag.error ~span:t.span "%s uses %S, which is a %s, not a library" t.name u
                (kind_name x.kind)
          | Some _ -> ())
        t.uses)
    targets;
  let rec cyc seen t =
    if List.mem t.name seen then
      Diag.error ~span:t.span "dependency cycle: %s" (String.concat " -> " (List.rev (t.name :: seen)));
    List.iter (fun u -> match find p u with Some x -> cyc (t.name :: seen) x | None -> ()) t.uses
  in
  List.iter (cyc []) targets;
  p
