type lang = C | Cxx | Asm

type tkind = Lib | Shared | Bin | Test

let kind_name = function Lib -> "lib" | Shared -> "shared" | Bin -> "bin" | Test -> "test"

type target = {
  name : string;
  kind : tkind;
  dir : string;
  srcs : string list;
  gen_srcs : string list;
  includes : string list;
  uses : string list;
  cflags : string list;
  cxxflags : string list;
  ldflags : string list;
  defines : string list;
  pkgs : string list;
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
  ranlib : string;
  target : string;
  sysroot : string;
  xflags : string list;
  cflags : string list;
  cxxflags : string list;
  ldflags : string list;
  builddir : string;
}

type install_item = { from : string; dest : string }

type define = { dname : string; dvalue : string option }

type project = {
  pname : string;
  tc : toolchain;
  targets : target list;
  rules : rule list;
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

let lang_of path =
  match String.lowercase_ascii (Filename.extension path) with
  | ".cc" | ".cpp" | ".cxx" | ".c++" | ".mm" -> Cxx
  | ".s" | ".asm" -> Asm
  | _ -> C

let all_srcs t = t.srcs @ t.gen_srcs
