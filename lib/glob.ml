let rec wildcard pat s pi si =
  let pn = String.length pat and sn = String.length s in
  if pi >= pn then si >= sn
  else
    match pat.[pi] with
    | '*' ->
        let rec try_at k = k <= sn && (wildcard pat s (pi + 1) k || try_at (k + 1)) in
        try_at si
    | '?' -> si < sn && wildcard pat s (pi + 1) (si + 1)
    | c -> si < sn && s.[si] = c && wildcard pat s (pi + 1) (si + 1)

let matches pat s = wildcard pat s 0 0
let has_magic s = String.exists (fun c -> c = '*' || c = '?') s

let entries dir =
  match Sys.readdir (if dir = "" then "." else dir) with
  | a ->
      let l = Array.to_list a in
      List.sort compare (List.filter (fun e -> e <> "" && e.[0] <> '.') l)
  | exception Sys_error _ -> []

let join dir name = if dir = "" then name else dir ^ "/" ^ name

let rec walk dir segs =
  match segs with
  | [] -> if Sys.file_exists (if dir = "" then "." else dir) then [ dir ] else []
  | "**" :: rest ->
      let here = walk dir rest in
      let subs = List.filter (fun e -> Fs.is_dir (join dir e)) (entries dir) in
      here @ List.concat_map (fun e -> walk (join dir e) ("**" :: rest)) subs
  | seg :: rest when has_magic seg ->
      entries dir
      |> List.filter (fun e -> matches seg e)
      |> List.concat_map (fun e -> walk (join dir e) rest)
  | seg :: rest ->
      let p = join dir seg in
      if Sys.file_exists (if p = "" then "." else p) then walk p rest else []

let expand pattern =
  if not (has_magic pattern) then if Sys.file_exists pattern then [ pattern ] else []
  else walk "" (String.split_on_char '/' pattern)

let rec seg_match ps ss =
  match (ps, ss) with
  | [], [] -> true
  | "**" :: pr, _ -> seg_match pr ss || (match ss with [] -> false | _ :: sr -> seg_match ps sr)
  | p :: pr, s :: sr -> matches p s && seg_match pr sr
  | _ -> false

let match_path pattern path =
  seg_match (String.split_on_char '/' pattern) (String.split_on_char '/' path)

let expand_with ~extra pattern =
  let real = expand pattern in
  let virt = if has_magic pattern then List.filter (match_path pattern) extra
             else List.filter (fun p -> p = pattern) extra in
  List.sort_uniq compare (real @ virt)
