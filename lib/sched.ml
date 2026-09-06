type result = { built : int; cached : int; failed : int; aborted : int; interrupted : bool }

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
  let cancelled = Hashtbl.create 8 in
  let built = ref 0 and cached = ref 0 and failed = ref 0 and aborted_count = ref 0 in
  let interrupted = ref false in
  let stopping () = !interrupted || (!failed > 0 && not keep_going) in
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
    flush stdout;
    let pid =
      match Unix.fork () with
      | 0 ->
          (try ignore (Unix.setsid ()) with Unix.Unix_error _ -> ());
          (try
             Unix.dup2 nul Unix.stdin;
             Unix.dup2 fd Unix.stdout;
             Unix.dup2 fd Unix.stderr;
             Unix.close fd;
             Unix.close nul;
             Unix.execvp v.cmd.(0) v.cmd
           with _ -> ());
          Unix._exit 127
      | pid -> pid
      | exception Unix.Unix_error (e, _, _) ->
          Unix.close fd;
          Unix.close nul;
          Diag.error "cannot fork for %s: %s" v.cmd.(0) (Unix.error_message e)
    in
    Unix.close fd;
    Unix.close nul;
    state.(v.id) <- Running;
    on_start v;
    Hashtbl.replace running pid (v, tmp)
  in
  let cancel_running () =
    Hashtbl.iter
      (fun pid _ ->
        Hashtbl.replace cancelled pid ();
        try Unix.kill (-pid) Sys.sigterm with Unix.Unix_error _ -> ())
      running
  in
  let discard (v : Graph.node) =
    List.iter Cache.forget v.outs;
    Cache.drop cache (List.hd v.outs);
    List.iter (fun o -> try Sys.remove o with Sys_error _ -> ()) v.outs;
    skip v.id
  in
  let rec wait_child () =
    match Unix.wait () with
    | r -> Some r
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait_child ()
    | exception Unix.Unix_error (Unix.ECHILD, _, _) -> None
  in
  let reap () =
    match wait_child () with
    | None -> Hashtbl.reset running
    | Some (pid, status) -> (
    match Hashtbl.find_opt running pid with
    | None -> ()
    | Some (v, tmp) ->
        Hashtbl.remove running pid;
        let aborted = Hashtbl.mem cancelled pid in
        Hashtbl.remove cancelled pid;
        let ok = status = Unix.WEXITED 0 in
        let log = try Fs.read tmp with Sys_error _ -> "" in
        (try Sys.remove tmp with Sys_error _ -> ());
        if ok then begin
          incr built;
          state.(v.id) <- Done;
          List.iter Cache.forget v.outs;
          (match v.depfile with Some d -> Cache.forget d | None -> ());
          Cache.put cache (List.hd v.outs) (key_of v);
          release v.id;
          on_done ~ok ~log v;
          if verbose then print_endline ("      " ^ Style.dim (Exec.show v.cmd))
        end
        else if aborted then begin
          incr aborted_count;
          discard v
        end
        else begin
          incr failed;
          discard v;
          on_done ~ok ~log v;
          if verbose then print_endline ("      " ^ Style.dim (Exec.show v.cmd));
          if not keep_going then cancel_running ()
        end)
  in
  let take_signals () =
    let handle _ =
      if not !interrupted then begin
        interrupted := true;
        Sys.set_signal Sys.sigint Sys.Signal_default;
        Sys.set_signal Sys.sigterm Sys.Signal_default;
        cancel_running ()
      end
    in
    (Sys.signal Sys.sigint (Sys.Signal_handle handle), Sys.signal Sys.sigterm (Sys.Signal_handle handle))
  in
  let prev_int, prev_term = take_signals () in
  let restore_signals () =
    Sys.set_signal Sys.sigint prev_int;
    Sys.set_signal Sys.sigterm prev_term
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
  restore_signals ();
  { built = !built; cached = !cached; failed = !failed; aborted = !aborted_count; interrupted = !interrupted }
