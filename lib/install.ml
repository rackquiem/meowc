open Types

let dest_dir p = function
  | "bin" -> Filename.concat p.prefix "bin"
  | "lib" -> Filename.concat p.prefix "lib"
  | "include" -> Filename.concat p.prefix "include"
  | other -> if Filename.is_relative other then Filename.concat p.prefix other else other

let plan p =
  let from_targets =
    List.filter_map
      (fun (t : target) ->
        match t.install with
        | None -> None
        | Some where ->
            let src = Build.out_of p t in
            Some (src, Filename.concat (dest_dir p where) (Filename.basename src)))
      p.targets
  in
  let from_blocks =
    List.map (fun i -> (i.from, Filename.concat (dest_dir p i.dest) (Filename.basename i.from)))
      p.installs
  in
  from_targets @ from_blocks

let copy ~mode src dst =
  Fs.mkdir_p (Filename.dirname dst);
  let ic = open_in_bin src in
  let n = in_channel_length ic in
  let data = really_input_string ic n in
  close_in ic;
  let oc = open_out_gen [ Open_wronly; Open_creat; Open_trunc; Open_binary ] mode dst in
  output_string oc data;
  close_out oc;
  Unix.chmod dst mode

let executable src =
  match Unix.stat src with
  | { Unix.st_perm; _ } -> st_perm land 0o111 <> 0
  | exception Unix.Unix_error _ -> false

let execute ?(dry = false) p =
  let items = plan p in
  List.iter
    (fun (src, dst) ->
      if not (Sys.file_exists src) then Diag.error "%s has not been built yet" src;
      if not dry then copy ~mode:(if executable src then 0o755 else 0o644) src dst)
    items;
  items
