type t = { path : string; keys : (string, string) Hashtbl.t }

let load path =
  let keys = Hashtbl.create 64 in
  (match Fs.read path with
  | exception Sys_error _ -> ()
  | raw ->
      String.split_on_char '\n' raw
      |> List.iter (fun line ->
             match String.index_opt line '\t' with
             | None -> ()
             | Some i ->
                 let key = String.sub line 0 i in
                 let out = String.sub line (i + 1) (String.length line - i - 1) in
                 if out <> "" then Hashtbl.replace keys out key));
  { path; keys }

let save c =
  let b = Buffer.create 4096 in
  Hashtbl.iter (fun out key -> Buffer.add_string b (key ^ "\t" ^ out ^ "\n")) c.keys;
  Fs.write c.path (Buffer.contents b)

let current c out = Hashtbl.find_opt c.keys out
let put c out key = Hashtbl.replace c.keys out key
let drop c out = Hashtbl.remove c.keys out

let digests : (string, string option) Hashtbl.t = Hashtbl.create 256

let digest_file p =
  match Hashtbl.find_opt digests p with
  | Some d -> d
  | None ->
      let d = match Digest.file p with d -> Some (Digest.to_hex d) | exception Sys_error _ -> None in
      Hashtbl.replace digests p d;
      d

let forget p = Hashtbl.remove digests p

let key ~cmd ~inputs =
  let b = Buffer.create 256 in
  Buffer.add_string b cmd;
  List.iter
    (fun p ->
      Buffer.add_char b '\000';
      Buffer.add_string b p;
      Buffer.add_char b '\000';
      Buffer.add_string b (match digest_file p with Some d -> d | None -> "?"))
    (List.sort compare inputs);
  Digest.to_hex (Digest.string (Buffer.contents b))
