type node = {
  id : int;
  tag : string;
  label : string;
  cmd : string array;
  outs : string list;
  ins : string list;
  depfile : string option;
  ords : string list;
  (* where to spill the arguments when the command line will not fit; only set
     for commands meowc composes itself, since an arbitrary program from a rule
     need not understand @file *)
  rsp : string option;
  mutable deps : int list;
}

type t = { nodes : node array; by_output : (string, int) Hashtbl.t }

let make ~id ~tag ~label ~cmd ~outs ~ins ?depfile ?(ords = []) ?rsp () =
  { id; tag; label; cmd; outs; ins; depfile; ords; rsp; deps = [] }

let build specs =
  let nodes = Array.of_list specs in
  Array.iteri (fun i n -> if n.id <> i then invalid_arg "graph: node ids must be dense") nodes;
  let by_output = Hashtbl.create (Array.length nodes * 2) in
  Array.iter
    (fun n ->
      List.iter
        (fun o ->
          match Hashtbl.find_opt by_output o with
          | Some prev when prev <> n.id ->
              Diag.error "two rules both produce %s (%s and %s)" o nodes.(prev).label n.label
          | _ -> Hashtbl.replace by_output o n.id)
        n.outs)
    nodes;
  Array.iter
    (fun n ->
      n.deps <-
        List.filter_map (fun i -> Hashtbl.find_opt by_output i) (n.ins @ n.ords)
        |> List.sort_uniq compare
        |> List.filter (fun d -> d <> n.id))
    nodes;
  { nodes; by_output }

let topo g =
  let n = Array.length g.nodes in
  let state = Array.make n 0 in
  let order = ref [] in
  let rec visit path i =
    match state.(i) with
    | 2 -> ()
    | 1 ->
        let cycle = List.rev_map (fun j -> g.nodes.(j).label) (i :: path) in
        Diag.error "dependency cycle: %s" (String.concat " -> " cycle)
    | _ ->
        state.(i) <- 1;
        List.iter (visit (i :: path)) g.nodes.(i).deps;
        state.(i) <- 2;
        order := i :: !order
  in
  for i = 0 to n - 1 do visit [] i done;
  List.rev !order

let reachable g roots =
  let seen = Hashtbl.create 64 in
  let rec go i =
    if not (Hashtbl.mem seen i) then begin
      Hashtbl.add seen i ();
      List.iter go g.nodes.(i).deps
    end
  in
  List.iter go roots;
  seen

let producing g path = Hashtbl.find_opt g.by_output path
