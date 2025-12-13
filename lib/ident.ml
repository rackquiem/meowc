let cident s =
  let b = Buffer.create (String.length s + 4) in
  String.iter
    (fun c ->
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> Buffer.add_char b (Char.uppercase_ascii c)
      | '*' -> Buffer.add_string b "_P"
      | _ -> Buffer.add_char b '_')
    s;
  let raw = Buffer.contents b in
  let parts = String.split_on_char '_' raw |> List.filter (fun p -> p <> "") in
  String.concat "_" parts

let lower s = String.lowercase_ascii (cident s)
let have s = "HAVE_" ^ cident s
let have_var s = "have_" ^ lower s
let sizeof_name s = "SIZEOF_" ^ cident s
let sizeof_var s = "sizeof_" ^ lower s
