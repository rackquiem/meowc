type job = { desc : string; cmd : string array; after : unit -> unit }

type report = { ran : int; failures : int }

let quote s =
  if s <> "" && String.for_all (fun c -> not (List.mem c [ ' '; '\t'; '"'; '\''; '$'; '&'; ';' ])) s
  then s
  else "'" ^ String.concat "'\\''" (String.split_on_char '\'' s) ^ "'"

let show cmd = String.concat " " (List.map quote (Array.to_list cmd))

let run ~jobs ~verbose ~on_done js =
  let queue = ref js in
  let running = Hashtbl.create 8 in
  let failures = ref 0 in
  let ran = ref 0 in
  let spawn j =
    let tmp = Filename.temp_file "meowc" ".log" in
    let fd = Unix.openfile tmp [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
    let pid =
      try Unix.create_process j.cmd.(0) j.cmd Unix.stdin fd fd
      with Unix.Unix_error (e, _, _) ->
        Unix.close fd;
        Sys.remove tmp;
        Diag.error "cannot run %s: %s" j.cmd.(0) (Unix.error_message e)
    in
    Unix.close fd;
    Hashtbl.replace running pid (j, tmp)
  in
  let reap () =
    let pid, status = Unix.wait () in
    match Hashtbl.find_opt running pid with
    | None -> ()
    | Some (j, tmp) ->
        Hashtbl.remove running pid;
        incr ran;
        let ok = status = Unix.WEXITED 0 in
        if ok then j.after () else incr failures;
        let log = try Fs.read tmp with Sys_error _ -> "" in
        (try Sys.remove tmp with Sys_error _ -> ());
        on_done ~ok j;
        if verbose then print_endline ("      " ^ Style.dim (show j.cmd));
        if String.trim log <> "" then (print_string log; flush stdout)
  in
  while !queue <> [] || Hashtbl.length running > 0 do
    while !queue <> [] && Hashtbl.length running < jobs && !failures = 0 do
      match !queue with
      | j :: rest -> queue := rest; spawn j
      | [] -> ()
    done;
    if Hashtbl.length running > 0 then reap ()
    else if !failures > 0 then queue := []
  done;
  { ran = !ran; failures = !failures }

let devnull () = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0

let capture cmd =
  let tmp = Filename.temp_file "meowc" ".out" in
  let fd = Unix.openfile tmp [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
  let nul = devnull () in
  let code =
    match Unix.create_process cmd.(0) cmd nul fd fd with
    | pid ->
        let _, st = Unix.waitpid [] pid in
        (match st with Unix.WEXITED c -> c | _ -> 128)
    | exception Unix.Unix_error _ -> 127
  in
  Unix.close fd;
  Unix.close nul;
  let out = try Fs.read tmp with Sys_error _ -> "" in
  (try Sys.remove tmp with Sys_error _ -> ());
  (code, out)

let ok cmd = fst (capture cmd) = 0

let which name =
  if String.contains name '/' then Sys.file_exists name
  else
    match Sys.getenv_opt "PATH" with
    | None -> false
    | Some path ->
        String.split_on_char ':' path
        |> List.exists (fun dir -> dir <> "" && Sys.file_exists (Filename.concat dir name))
