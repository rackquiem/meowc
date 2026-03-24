let of_file path =
  match Fs.read path with
  | exception Sys_error _ -> None
  | raw ->
      let b = Buffer.create (String.length raw) in
      let n = String.length raw in
      let i = ref 0 in
      while !i < n do
        if raw.[!i] = '\\' && !i + 1 < n && raw.[!i + 1] = '\n' then (Buffer.add_char b ' '; i := !i + 2)
        else if raw.[!i] = '\n' then (Buffer.add_char b ' '; incr i)
        else (Buffer.add_char b raw.[!i]; incr i)
      done;
      let flat = Buffer.contents b in
      match String.index_opt flat ':' with
      | None -> None
      | Some c ->
          let rhs = String.sub flat (c + 1) (String.length flat - c - 1) in
          let parts =
            String.split_on_char ' ' rhs
            |> List.map String.trim
            |> List.filter (fun s -> s <> "" && s <> "\\")
          in
          Some parts
