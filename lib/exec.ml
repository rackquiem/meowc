let quote s =
  if s <> "" && String.for_all (fun c -> not (List.mem c [ ' '; '\t'; '"'; '\''; '$'; '&'; ';' ])) s
  then s
  else "'" ^ String.concat "'\\''" (String.split_on_char '\'' s) ^ "'"

let show cmd = String.concat " " (List.map quote (Array.to_list cmd))

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

(* execve measures the arguments and the environment together against ARG_MAX,
   so what a command line may use is the limit less the environment it inherits.
   Staying under a fraction of that leaves room for the pointer table and for an
   environment that grows between the check and the spawn. *)
let arg_max =
  lazy
    (match capture [| "getconf"; "ARG_MAX" |] with
    | 0, out -> ( match int_of_string_opt (String.trim out) with Some n when n > 0 -> n | _ -> 131072)
    | _ -> 131072
    | exception _ -> 131072)

let entry_size s = String.length s + 1 + 8

let budget =
  lazy
    (let env = Array.fold_left (fun n s -> n + entry_size s) 0 (Unix.environment ()) in
     max 8192 ((Lazy.force arg_max - env) * 7 / 8))

let too_long cmd = Array.fold_left (fun n s -> n + entry_size s) 0 cmd > Lazy.force budget

(* A GNU response file holds one argument per line, quoted so that spaces and
   backslashes in a path survive the second round of parsing. The compiler
   drivers and binutils expand @file before they read anything else, so the tool
   sees the same argument list either way. *)
let quote_arg s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter (fun c -> if c = '"' || c = '\\' then Buffer.add_char b '\\'; Buffer.add_char b c) s;
  Buffer.add_char b '"';
  Buffer.contents b

let response path cmd =
  let args = Array.to_list (Array.sub cmd 1 (Array.length cmd - 1)) in
  Fs.write path (String.concat "\n" (List.map quote_arg args) ^ "\n");
  [| cmd.(0); "@" ^ path |]
