open Types

let sanitize s =
  String.concat "/" (List.map (fun p -> if p = ".." then "__" else p) (String.split_on_char '/' s))

let obj_of p (t : target) src = p.tc.builddir ^ "/obj/" ^ t.name ^ "/" ^ sanitize src ^ ".o"
let fmt p = Binfmt.of_platform p.platform
let exe_ext p = Binfmt.exe_ext (fmt p)
let lib_of p (t : target) = p.tc.builddir ^ "/" ^ Binfmt.static_name t.name
let so_of p (t : target) = p.tc.builddir ^ "/" ^ Binfmt.shared_name (fmt p) t.name

(* Formats that cannot be linked against directly leave an import library
   beside the shared one, which is what dependents resolve -l against. *)
let implib_of p (t : target) =
  if t.kind <> Shared then None
  else
    Option.map (fun n -> p.tc.builddir ^ "/" ^ n) (Binfmt.import_name (fmt p) t.name)

let bin_of p (t : target) = p.tc.builddir ^ "/bin/" ^ t.name ^ exe_ext p
let test_of p (t : target) = p.tc.builddir ^ "/test/" ^ t.name ^ exe_ext p

let out_of p (t : target) =
  match t.kind with
  | Lib -> lib_of p t
  | Shared -> so_of p t
  | Bin -> bin_of p t
  | Test -> test_of p t

let pic_set p =
  let set = Hashtbl.create 8 in
  List.iter
    (fun t ->
      if t.kind = Shared then begin
        Hashtbl.replace set t.name ();
        List.iter (fun (d : target) -> Hashtbl.replace set d.name ()) (deps_of p t)
      end)
    p.targets;
  set

let config_dir p = match p.config_header with None -> [] | Some h -> [ Filename.dirname h ]

let includes_of p (t : target) =
  let inherited = List.concat_map (fun (d : target) -> d.includes) (deps_of p t) in
  List.sort_uniq compare (t.includes @ inherited @ config_dir p)

(* A compiler only writes a depfile when it preprocesses, which for assembly
   means .S and not .s. Asking for one that never appears would leave the action
   permanently out of date. *)
let tracks_deps src =
  match lang_of src with Asm -> Filename.extension src = ".S" | C | Cxx -> true

let compile_cmd p ~pic (t : target) src obj =
  let lang = lang_of src in
  let driver = match lang with Cxx -> p.tc.cxx | _ -> p.tc.cc in
  let base = p.tc.xflags @ (match lang with Cxx -> p.tc.cxxflags @ t.cxxflags | _ -> p.tc.cflags @ t.cflags) in
  let inherited_flags =
    List.concat_map
      (fun (d : target) -> match lang with Cxx -> d.cxxflags | _ -> d.cflags)
      (deps_of p t)
    |> List.filter (fun f -> String.length f > 1 && String.sub f 0 2 = "-I")
  in
  let flags =
    base @ inherited_flags
    @ (if Hashtbl.mem pic t.name then Binfmt.pic_flags (fmt p) else [])
    @ List.concat_map (fun d -> [ "-D"; d ]) t.defines
    @ List.concat_map (fun d -> [ "-I"; d ]) (includes_of p t)
  in
  let dep = if tracks_deps src then [ "-MMD"; "-MF"; obj ^ ".d" ] else [] in
  Array.of_list (((driver :: flags) @ dep) @ [ "-c"; src; "-o"; obj ])

let link_libs p (t : target) =
  let deps = List.rev (deps_of p t) in
  (if deps = [] then [] else [ "-L"; p.tc.builddir ])
  @ List.map (fun (d : target) -> "-l" ^ d.name) deps
  @ List.concat_map (fun (d : target) -> d.ldflags) deps

let has_cxx p (t : target) =
  List.exists (fun s -> lang_of s = Cxx) (all_srcs t)
  || List.exists (fun (d : target) -> List.exists (fun s -> lang_of s = Cxx) (all_srcs d)) (deps_of p t)

let link_cmd p (t : target) objs =
  let driver = if has_cxx p t then p.tc.cxx else p.tc.cc in
  let out = out_of p t in
  match t.kind with
  | Lib -> Array.of_list ((p.tc.ar :: [ "rcs"; out ]) @ objs)
  | Shared ->
      let soname =
        match t.soname with None -> [] | Some s -> Binfmt.soname_flags (fmt p) s
      in
      let implib =
        match implib_of p t with None -> [] | Some f -> Binfmt.implib_flags (fmt p) f
      in
      Array.of_list
        (((driver :: Binfmt.shared_flags (fmt p)) @ objs @ [ "-o"; out ]) @ soname @ implib
       @ link_libs p t @ t.ldflags @ p.tc.ldflags @ p.tc.xflags)
  | Bin | Test ->
      Array.of_list (((driver :: objs) @ [ "-o"; out ]) @ link_libs p t @ t.ldflags @ p.tc.ldflags @ p.tc.xflags)

let tag_of = function Lib -> "ar" | Shared -> "so" | Bin | Test -> "ld"

let is_header f =
  List.mem (String.lowercase_ascii (Filename.extension f)) [ ".h"; ".hh"; ".hpp"; ".hxx"; ".inc" ]

type t = { g : Graph.t; outputs : (string, int) Hashtbl.t; p : project }

let of_project p =
  let pic = pic_set p in
  let nodes = ref [] in
  let n = ref 0 in
  let add spec = nodes := spec :: !nodes; incr n in
  let gen_headers = List.concat_map (fun r -> List.filter is_header r.routs) p.rules in
  List.iter
    (fun r ->
      let cmd = Array.of_list r.rcmd in
      (* a tool the rule runs is an input like any other, so building it first is
         ordering and rebuilding it is invalidation; a script is already a file *)
      let tools = List.map (fun u -> match find p u with Some t -> out_of p t | None -> u) r.ruses in
      add
        (Graph.make ~id:!n ~tag:"gen" ~label:r.rdesc ~cmd ~outs:r.routs ~ins:(r.rin @ tools) ()))
    p.rules;
  let outputs = Hashtbl.create 16 in
  List.iter
    (fun (t : target) ->
      let objs =
        List.map
          (fun src ->
            let obj = obj_of p t src in
            let cmd = compile_cmd p ~pic t src obj in
            let tag = match lang_of src with Cxx -> "c++" | Asm -> "as" | C -> "cc" in
            let depfile = if tracks_deps src then Some (obj ^ ".d") else None in
            add
              (Graph.make ~id:!n ~tag ~label:src ~cmd ~outs:[ obj ] ~ins:[ src ] ?depfile
                 ~ords:gen_headers ~rsp:(obj ^ ".rsp") ());
            obj)
          (all_srcs t)
      in
      let out = out_of p t in
      let tag = tag_of t.kind in
      let ins = objs @ List.map (fun (d : target) -> out_of p d) (deps_of p t) in
      let label = match t.kind with Lib | Shared -> Filename.basename out | _ -> t.name in
      let outs = out :: Option.to_list (implib_of p t) in
      add
        (Graph.make ~id:!n ~tag ~label ~cmd:(link_cmd p t objs) ~outs ~ins ~rsp:(out ^ ".rsp") ());
      Hashtbl.replace outputs t.name (!n - 1))
    p.targets;
  { g = Graph.build (List.rev !nodes); outputs; p }

let select b targets =
  let roots = List.filter_map (fun (t : target) -> Hashtbl.find_opt b.outputs t.name) targets in
  Graph.reachable b.g roots

let paint tag text =
  match tag with
  | "cc" -> Style.blue text
  | "c++" -> Style.blue text
  | "as" -> Style.magenta text
  | "ar" -> Style.magenta text
  | "so" -> Style.cyan text
  | "ld" -> Style.green text
  | "gen" -> Style.yellow text
  | _ -> text
