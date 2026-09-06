open Types

(* The stage shows in the outline rather than in colour: a generated file gets a
   document shape, and what the build actually ships gets a double rule. *)
let outline = function
  | "gen" -> "shape=note"
  | "ar" | "so" | "ld" -> "shape=box, peripheries=2"
  | _ -> "shape=box"

let serif = "Latin Modern Roman,CMU Serif,Nimbus Roman,Times New Roman,serif"

let preamble =
  Printf.sprintf
    "  rankdir=LR;\n  bgcolor=\"white\";\n\
    \  node [style=filled, fillcolor=\"white\", color=\"black\", penwidth=0.8, fontname=%S, \
     fontsize=10, fontcolor=\"black\"];\n\
    \  edge [color=\"black\", penwidth=0.7, arrowsize=0.6];\n"
    serif

let render ?(nodes = true) ?selected (b : Build.t) =
  let keep (n : Graph.node) =
    match selected with None -> true | Some s -> Hashtbl.mem s n.id
  in
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "digraph meowc {\n";
  Buffer.add_string buf preamble;
  if nodes then
    Array.iter
      (fun (n : Graph.node) ->
        if keep n then
        Buffer.add_string buf
          (Printf.sprintf "  n%d [label=%s, %s];\n" n.id
             (Json.str (n.tag ^ "  " ^ Filename.basename n.label))
             (outline n.tag)))
      b.g.Graph.nodes;
  Array.iter
    (fun (n : Graph.node) ->
      if keep n then
        List.iter
          (fun d ->
            if keep b.g.Graph.nodes.(d) then
              Buffer.add_string buf (Printf.sprintf "  n%d -> n%d;\n" d n.id))
          n.deps)
    b.g.Graph.nodes;
  Buffer.add_string buf "}\n";
  Buffer.contents buf

let targets_only (p : project) =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "digraph targets {\n";
  Buffer.add_string buf preamble;
  List.iter
    (fun (t : target) ->
      Buffer.add_string buf
        (Printf.sprintf "  %s [label=%s, %s];\n" (Json.str t.name)
           (Json.str (kind_name t.kind ^ "  " ^ t.name))
           (outline (Build.tag_of t.kind)));
      List.iter
        (fun u -> Buffer.add_string buf (Printf.sprintf "  %s -> %s;\n" (Json.str t.name) (Json.str u)))
        t.uses)
    p.targets;
  Buffer.add_string buf "}\n";
  Buffer.contents buf
