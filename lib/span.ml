type t = { file : string; line : int; col : int; len : int }

let none = { file = ""; line = 0; col = 0; len = 0 }
let is_none s = s.line = 0
let make file line col len = { file; line; col; len }

let to_string s =
  if is_none s then "" else Printf.sprintf "%s:%d:%d" s.file s.line s.col

let join a b =
  if is_none a then b
  else if is_none b then a
  else if a.line <> b.line then a
  else { a with len = max a.len (b.col + b.len - a.col) }
