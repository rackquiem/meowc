open Types

let color = function
  | "cc" | "c++" -> "#4c78a8"
  | "ar" -> "#b279a2"
  | "so" -> "#72b7b2"
  | "ld" -> "#54a24b"
  | "gen" -> "#eeca3b"
  | _ -> "#888888"

let render ?(nodes = true) (b : Build.t) =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "digraph meowc {\n";
  Buffer.add_string buf "  rankdir=LR;\n  bgcolor=\"transparent\";\n";
  Buffer.add_string buf
    "  node [shape=box, style=\"rounded,filled\", fontname=\"Inter,Helvetica,sans-serif\", fontsize=10, color=\"#00000000\", fontcolor=\"#ffffff\"];\n";
  Buffer.add_string buf "  edge [color=\"#999999\", arrowsize=0.6];\n";
  if nodes then
    Array.iter
      (fun (n : Graph.node) ->
        Buffer.add_string buf
          (Printf.sprintf "  n%d [label=%s, fillcolor=%s];\n" n.id
             (Json.str (n.tag ^ "  " ^ Filename.basename n.label))
             (Json.str (color n.tag))))
      b.g.Graph.nodes;
  Array.iter
    (fun (n : Graph.node) ->
      List.iter (fun d -> Buffer.add_string buf (Printf.sprintf "  n%d -> n%d;\n" d n.id)) n.deps)
    b.g.Graph.nodes;
  Buffer.add_string buf "}\n";
  Buffer.contents buf

let targets_only (p : project) =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "digraph targets {\n  rankdir=LR;\n";
  List.iter
    (fun (t : target) ->
      Buffer.add_string buf
        (Printf.sprintf "  %s [label=%s];\n" (Json.str t.name)
           (Json.str (kind_name t.kind ^ " " ^ t.name)));
      List.iter
        (fun u -> Buffer.add_string buf (Printf.sprintf "  %s -> %s;\n" (Json.str t.name) (Json.str u)))
        t.uses)
    p.targets;
  Buffer.add_string buf "}\n";
  Buffer.contents buf
