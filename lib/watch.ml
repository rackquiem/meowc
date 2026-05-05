let snapshot files =
  List.filter_map (fun f -> match Fs.mtime f with Some m -> Some (f, m) | None -> None) files

let inputs (b : Build.t) =
  let outs = Hashtbl.create 64 in
  Array.iter (fun (n : Graph.node) -> List.iter (fun o -> Hashtbl.replace outs o ()) n.outs) b.g.Graph.nodes;
  let acc = ref [] in
  Array.iter
    (fun (n : Graph.node) ->
      List.iter (fun i -> if not (Hashtbl.mem outs i) then acc := i :: !acc) (Sched.inputs_of n))
    b.g.Graph.nodes;
  List.sort_uniq compare !acc

let changed before after =
  let tbl = Hashtbl.create 64 in
  List.iter (fun (f, m) -> Hashtbl.replace tbl f m) before;
  List.filter (fun (f, m) -> match Hashtbl.find_opt tbl f with Some p -> p <> m | None -> true) after
  |> List.map fst

let wait ~interval files =
  let before = snapshot files in
  let rec loop () =
    Unix.sleepf interval;
    let now = snapshot files in
    match changed before now with [] -> loop () | cs -> cs
  in
  loop ()
