let render (b : Build.t) =
  let dir = Sys.getcwd () in
  let entries =
    Array.to_list b.g.Graph.nodes
    |> List.filter (fun (n : Graph.node) -> n.tag = "cc" || n.tag = "c++")
    |> List.map (fun (n : Graph.node) ->
           Printf.sprintf
             "  {\n    \"directory\": %s,\n    \"file\": %s,\n    \"output\": %s,\n    \"arguments\": %s\n  }"
             (Json.str dir) (Json.str n.label)
             (Json.str (List.hd n.outs))
             (Json.list Json.str (Array.to_list n.cmd)))
  in
  "[\n" ^ String.concat ",\n" entries ^ "\n]\n"

let write b path =
  Fs.write path (render b);
  List.length
    (List.filter (fun (n : Graph.node) -> n.Graph.tag = "cc" || n.Graph.tag = "c++")
       (Array.to_list b.Build.g.Graph.nodes))
