let rec mkdir_p dir =
  if dir <> "" && dir <> "." && dir <> "/" && not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let is_dir p = try Sys.is_directory p with Sys_error _ -> false

let read path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let write path s =
  mkdir_p (Filename.dirname path);
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc

let rec rm_rf p =
  if is_dir p then begin
    Array.iter (fun e -> rm_rf (Filename.concat p e)) (Sys.readdir p);
    try Unix.rmdir p with Unix.Unix_error _ -> ()
  end
  else if Sys.file_exists p then try Sys.remove p with Sys_error _ -> ()

let mtime p = try Some (Unix.stat p).Unix.st_mtime with Unix.Unix_error _ -> None
