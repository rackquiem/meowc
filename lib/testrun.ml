open Types

type outcome = { name : string; ok : bool; ms : float; output : string; code : int }

let run_one p (t : target) =
  let exe = Build.test_of p t in
  let t0 = Unix.gettimeofday () in
  let code, out = Exec.capture (Array.of_list (exe :: t.args)) in
  let ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
  { name = t.name; ok = code = 0; ms; output = out; code }

let tests p = List.filter (fun (t : target) -> t.kind = Test) p.targets
