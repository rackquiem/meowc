type result = { built : int; cached : int; failed : int }

type state = Blocked | Ready | Running | Done | Failed

let inputs_of (n : Graph.node) =
  match n.depfile with
  | None -> n.ins
  | Some d -> (
      match Dep.of_file d with
      | Some l -> List.sort_uniq compare (n.ins @ l)
      | None -> n.ins)

let key_of n = Cache.key ~cmd:(Exec.show n.Graph.cmd) ~inputs:(inputs_of n)

let up_to_date cache (n : Graph.node) =
  let outputs_present = List.for_all Sys.file_exists n.Graph.outs in
  let have_deps = match n.Graph.depfile with None -> true | Some d -> Sys.file_exists d in
  outputs_present && have_deps && Cache.current cache (List.hd n.Graph.outs) = Some (key_of n)

let run g ~selected ~jobs ~cache ~verbose ~keep_going ~on_start ~on_done =
  let nodes = g.Graph.nodes in
  let n = Array.length nodes in
  let state = Array.make n Blocked in
  let pending = Array.make n 0 in
  let rdeps = Array.make n [] in
  Array.iter
    (fun (v : Graph.node) ->
      pending.(v.id) <- List.length v.deps;
      List.iter (fun d -> rdeps.(d) <- v.id :: rdeps.(d)) v.deps)
    nodes;
  let wanted i = Hashtbl.mem selected i in
  let ready = Queue.create () in
  Array.iter (fun (v : Graph.node) -> if wanted v.id && pending.(v.id) = 0 then Queue.add v.id ready) nodes;
  let running = Hashtbl.create 8 in
  let built = ref 0 and cached = ref 0 and failed = ref 0 in
  let stopping () = !failed > 0 && not keep_going in
  let release i =
    List.iter
      (fun j ->
        if wanted j then begin
          pending.(j) <- pending.(j) - 1;
          if pending.(j) = 0 && state.(j) = Blocked then (state.(j) <- Ready; Queue.add j ready)
        end)
      rdeps.(i)
  in
  let skip i =
    state.(i) <- Failed;
    List.iter (fun j -> if wanted j && state.(j) = Blocked then pending.(j) <- pending.(j) - 1) rdeps.(i)
  in
  let spawn (v : Graph.node) =
    List.iter (fun o -> Fs.mkdir_p (Filename.dirname o)) v.outs;
    let tmp = Filename.temp_file "meowc" ".log" in
    let fd = Unix.openfile tmp [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
    let nul = Exec.devnull () in
    let pid =
      match Unix.create_process v.cmd.(0) v.cmd nul fd fd with
      | pid -> pid
      | exception Unix.Unix_error (e, _, _) ->
          Unix.close fd;
          Unix.close nul;
          Diag.error "cannot run %s: %s" v.cmd.(0) (Unix.error_message e)
    in
    Unix.close fd;
    Unix.close nul;
    state.(v.id) <- Running;
    on_start v;
    Hashtbl.replace running pid (v, tmp)
  in
  let reap () =
    let pid, status = Unix.wait () in
    match Hashtbl.find_opt running pid with
    | None -> ()
    | Some (v, tmp) ->
        Hashtbl.remove running pid;
        let ok = status = Unix.WEXITED 0 in
        let log = try Fs.read tmp with Sys_error _ -> "" in
        (try Sys.remove tmp with Sys_error _ -> ());
        if ok then begin
          incr built;
          state.(v.id) <- Done;
          Cache.put cache (List.hd v.outs) (key_of v);
          release v.id
        end
        else begin
          incr failed;
          Cache.drop cache (List.hd v.outs);
          List.iter (fun o -> try Sys.remove o with Sys_error _ -> ()) v.outs;
          skip v.id
        end;
        on_done ~ok ~log v;
        if verbose then print_endline ("      " ^ Style.dim (Exec.show v.cmd))
  in
  while (not (Queue.is_empty ready) && not (stopping ())) || Hashtbl.length running > 0 do
    while (not (Queue.is_empty ready)) && Hashtbl.length running < jobs && not (stopping ()) do
      let i = Queue.pop ready in
      let v = nodes.(i) in
      if up_to_date cache v then begin
        incr cached;
        state.(i) <- Done;
        release i
      end
      else spawn v
    done;
    if Hashtbl.length running > 0 then reap ()
    else if Queue.is_empty ready then ()
  done;
  { built = !built; cached = !cached; failed = !failed }
