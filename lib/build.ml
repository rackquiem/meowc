open Types

let sanitize s =
  String.concat "/" (List.map (fun p -> if p = ".." then "__" else p) (String.split_on_char '/' s))

let obj_of p (t : target) src = p.tc.builddir ^ "/obj/" ^ t.name ^ "/" ^ sanitize src ^ ".o"
let lib_of p (t : target) = p.tc.builddir ^ "/lib" ^ t.name ^ ".a"
let dylib_ext p =
  match p.platform with "darwin" -> ".dylib" | "windows" -> ".dll" | _ -> ".so"

let exe_ext p = if p.platform = "windows" then ".exe" else ""
let so_of p (t : target) = p.tc.builddir ^ "/lib" ^ t.name ^ dylib_ext p
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
    @ (if Hashtbl.mem pic t.name then [ "-fPIC" ] else [])
    @ List.concat_map (fun d -> [ "-D"; d ]) t.defines
    @ List.concat_map (fun d -> [ "-I"; d ]) (includes_of p t)
  in
  Array.of_list (((driver :: flags) @ [ "-MMD"; "-MF"; obj ^ ".d" ]) @ [ "-c"; src; "-o"; obj ])

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
        match t.soname with
        | None -> []
        | Some s ->
            if p.platform = "darwin" then [ "-Wl,-install_name," ^ s ] else [ "-Wl,-soname," ^ s ]
      in
      Array.of_list
        (((driver :: [ "-shared" ]) @ objs @ [ "-o"; out ]) @ soname @ link_libs p t @ t.ldflags
       @ p.tc.ldflags @ p.tc.xflags)
  | Bin | Test ->
      Array.of_list (((driver :: objs) @ [ "-o"; out ]) @ link_libs p t @ t.ldflags @ p.tc.ldflags @ p.tc.xflags)

let tag_of = function
  | Lib -> ("ar", Style.magenta)
  | Shared -> ("so", Style.cyan)
  | Bin -> ("ld", Style.green)
  | Test -> ("ld", Style.green)

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
      add
        (Graph.make ~id:!n ~tag:"gen" ~label:r.rdesc ~cmd ~outs:r.routs ~ins:r.rin ()))
    p.rules;
  let outputs = Hashtbl.create 16 in
  List.iter
    (fun (t : target) ->
      let objs =
        List.map
          (fun src ->
            let obj = obj_of p t src in
            let cmd = compile_cmd p ~pic t src obj in
            let tag = if lang_of src = Cxx then "c++" else "cc" in
            add
              (Graph.make ~id:!n ~tag ~label:src ~cmd ~outs:[ obj ] ~ins:[ src ]
                 ~depfile:(obj ^ ".d") ~ords:gen_headers ());
            obj)
          (all_srcs t)
      in
      let out = out_of p t in
      let tag, _ = tag_of t.kind in
      let ins = objs @ List.map (fun (d : target) -> out_of p d) (deps_of p t) in
      let label = match t.kind with Lib | Shared -> Filename.basename out | _ -> t.name in
      add (Graph.make ~id:!n ~tag ~label ~cmd:(link_cmd p t objs) ~outs:[ out ] ~ins ());
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
  | "ar" -> Style.magenta text
  | "so" -> Style.cyan text
  | "ld" -> Style.green text
  | "gen" -> Style.yellow text
  | _ -> text
