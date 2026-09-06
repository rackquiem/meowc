open Types

type outcome = { name : string; ok : bool; ms : float; output : string; code : int }

let tests p = List.filter (fun (t : target) -> t.kind = Test) p.targets

let failed name code = { name; ok = false; ms = 0.0; output = ""; code }

(* Tests are independent processes, so they run the same way the build does:
   up to jobs at once, each with its own output, reported in declaration order. *)
let run_all ~jobs p ts =
  let results = Hashtbl.create 8 in
  let running = Hashtbl.create 8 in
  let queue = ref ts in
  let spawn (t : target) =
    let exe = Build.test_of p t in
    let tmp = Filename.temp_file "meowc-test" ".log" in
    let fd = Unix.openfile tmp [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
    let nul = Exec.devnull () in
    let argv = Array.of_list (exe :: t.args) in
    (match Unix.create_process exe argv nul fd fd with
    | pid -> Hashtbl.replace running pid (t, tmp, Unix.gettimeofday ())
    | exception Unix.Unix_error _ ->
        (try Sys.remove tmp with Sys_error _ -> ());
        Hashtbl.replace results t.name (failed t.name 127));
    Unix.close fd;
    Unix.close nul
  in
  let reap () =
    match Unix.wait () with
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> ()
    | exception Unix.Unix_error (Unix.ECHILD, _, _) -> Hashtbl.reset running
    | pid, status -> (
        match Hashtbl.find_opt running pid with
        | None -> ()
        | Some (t, tmp, t0) ->
            Hashtbl.remove running pid;
            let ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
            let output = try Fs.read tmp with Sys_error _ -> "" in
            (try Sys.remove tmp with Sys_error _ -> ());
            let code = match status with Unix.WEXITED c -> c | _ -> 128 in
            Hashtbl.replace results t.name { name = t.name; ok = code = 0; ms; output; code })
  in
  while !queue <> [] || Hashtbl.length running > 0 do
    while !queue <> [] && Hashtbl.length running < jobs do
      match !queue with t :: rest -> queue := rest; spawn t | [] -> ()
    done;
    if Hashtbl.length running > 0 then reap ()
  done;
  List.map
    (fun (t : target) ->
      match Hashtbl.find_opt results t.name with Some o -> o | None -> failed t.name 127)
    ts
