let distance a b =
  let la = String.length a and lb = String.length b in
  if la = 0 then lb
  else if lb = 0 then la
  else begin
    let prev = Array.init (lb + 1) (fun j -> j) in
    let cur = Array.make (lb + 1) 0 in
    for i = 1 to la do
      cur.(0) <- i;
      for j = 1 to lb do
        let cost = if a.[i - 1] = b.[j - 1] then 0 else 1 in
        cur.(j) <- min (min (cur.(j - 1) + 1) (prev.(j) + 1)) (prev.(j - 1) + cost)
      done;
      Array.blit cur 0 prev 0 (lb + 1)
    done;
    prev.(lb)
  end

let closest word candidates =
  let limit = 1 + String.length word / 3 in
  List.fold_left
    (fun best c ->
      let d = distance word c in
      match best with
      | Some (_, bd) when bd <= d -> best
      | _ -> if d <= limit then Some (c, d) else best)
    None candidates
  |> Option.map fst

let hint word candidates =
  match closest word candidates with
  | Some c -> Printf.sprintf "did you mean %S?" c
  | None -> Printf.sprintf "expected one of: %s" (String.concat ", " candidates)
