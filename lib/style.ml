let enabled = ref true

let wrap code s = if !enabled then "\027[" ^ code ^ "m" ^ s ^ "\027[0m" else s

let bold s = wrap "1" s
let dim s = wrap "2" s
let red s = wrap "31" s
let green s = wrap "32" s
let yellow s = wrap "33" s
let blue s = wrap "34" s
let magenta s = wrap "35" s
let cyan s = wrap "36" s

let visible s =
  let n = String.length s in
  let rec go i acc =
    if i >= n then acc
    else if s.[i] = '\027' then
      let rec skip j = if j >= n || s.[j] = 'm' then j + 1 else skip (j + 1) in
      go (skip i) acc
    else go (i + 1) (acc + 1)
  in
  go 0 0

let pad n s = s ^ String.make (max 0 (n - visible s)) ' '

let pad_left n s = String.make (max 0 (n - visible s)) ' ' ^ s
let plural n w =
  let last = if w = "" then ' ' else w.[String.length w - 1] in
  let prev = if String.length w < 2 then ' ' else w.[String.length w - 2] in
  let form =
    if n = 1 then w
    else if last = 'y' && not (List.mem prev [ 'a'; 'e'; 'i'; 'o'; 'u' ]) then
      String.sub w 0 (String.length w - 1) ^ "ies"
    else if List.mem last [ 's'; 'x'; 'z' ] then w ^ "es"
    else w ^ "s"
  in
  Printf.sprintf "%d %s" n form
