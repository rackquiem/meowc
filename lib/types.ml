type lang = C | Cxx | Asm

type tkind = Lib | Shared | Bin | Test

let kind_name = function Lib -> "lib" | Shared -> "shared" | Bin -> "bin" | Test -> "test"

type target = {
  name : string;
  kind : tkind;
  srcs : string list;
  gen_srcs : string list;
  includes : string list;
  uses : string list;
  cflags : string list;
  cxxflags : string list;
  ldflags : string list;
  defines : string list;
  install : string option;
  soname : string option;
  args : string list;
  span : Span.t;
}

type rule = {
  rname : string;
  rin : string list;
  routs : string list;
  rcmd : string list;
  rdesc : string;
  rspan : Span.t;
}

type toolchain = {
  cc : string;
  cxx : string;
  ar : string;
  target : string;
  sysroot : string;
  xflags : string list;
  cflags : string list;
  cxxflags : string list;
  ldflags : string list;
  builddir : string;
}

(* A command the project can be asked to run, once whatever it names has been
   built. Unlike a rule it produces no file, so it is never part of the graph. *)
type script = { sname : string; suses : string list; scmd : string list; sspan : Span.t }

type install_item = { from : string; dest : string }

type define = { dname : string; dvalue : string option }

type project = {
  pname : string;
  tc : toolchain;
  targets : target list;
  rules : rule list;
  scripts : script list;
  defines : define list;
  config_header : string option;
  installs : install_item list;
  prefix : string;
  platform : string;
}

let find p name = List.find_opt (fun t -> t.name = name) p.targets

let rec deps_of p t =
  List.concat_map
    (fun u -> match find p u with None -> [] | Some x -> deps_of p x @ [ x ])
    t.uses

let uniq_targets ts =
  let seen = Hashtbl.create 8 in
  List.filter (fun t -> if Hashtbl.mem seen t.name then false else (Hashtbl.add seen t.name (); true)) ts

let order p ts = uniq_targets (List.concat_map (fun t -> deps_of p t @ [ t ]) ts)

let langs =
  [ (".c", C); (".m", C);
    (".cc", Cxx); (".cpp", Cxx); (".cxx", Cxx); (".c++", Cxx); (".mm", Cxx);
    (".s", Asm); (".asm", Asm) ]

let source_exts = List.map fst langs
let lang_opt path = List.assoc_opt (String.lowercase_ascii (Filename.extension path)) langs
let lang_of path = Option.value (lang_opt path) ~default:C

let all_srcs t = t.srcs @ t.gen_srcs
