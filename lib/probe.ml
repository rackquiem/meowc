type outcome = { ok : bool; value : string; note : string }

type t = {
  mutable cc : string;
  mutable cflags : string list;
  mutable ldflags : string list;
  mutable path : string;
  memo : (string, outcome) Hashtbl.t;
  mutable dirty : bool;
  mutable ran : int;
  mutable hits : int;
}

let load path =
  let memo = Hashtbl.create 64 in
  (match Fs.read path with
  | exception Sys_error _ -> ()
  | raw ->
      String.split_on_char '\n' raw
      |> List.iter (fun line ->
             match String.split_on_char '\t' line with
             | [ key; ok; value; note ] ->
                 Hashtbl.replace memo key { ok = ok = "1"; value; note }
             | _ -> ()));
  { cc = "cc"; cflags = []; ldflags = []; path; memo; dirty = false; ran = 0; hits = 0 }

let save t =
  if t.dirty then begin
    let b = Buffer.create 4096 in
    Hashtbl.iter
      (fun k o ->
        Buffer.add_string b
          (Printf.sprintf "%s\t%s\t%s\t%s\n" k (if o.ok then "1" else "0") o.value o.note))
      t.memo;
    Fs.write t.path (Buffer.contents b)
  end

let signature t = String.concat " " ((t.cc :: t.cflags) @ t.ldflags)

let cached t desc f =
  let key = Digest.to_hex (Digest.string (desc ^ "\000" ^ signature t)) in
  match Hashtbl.find_opt t.memo key with
  | Some o -> t.hits <- t.hits + 1; o
  | None ->
      t.ran <- t.ran + 1;
      let o = f () in
      Hashtbl.replace t.memo key o;
      t.dirty <- true;
      o

let with_temp_source src f =
  let dir = Filename.get_temp_dir_name () in
  let base = Filename.concat dir (Printf.sprintf "meowc-probe-%d" (Unix.getpid ())) in
  let c = base ^ ".c" in
  let exe = base ^ ".out" in
  Fs.write c src;
  let r = f c exe in
  List.iter (fun p -> try Sys.remove p with Sys_error _ -> ()) [ c; exe ];
  r

let compiles t ?(link = false) ?(libs = []) src =
  with_temp_source src (fun c exe ->
      let cmd =
        if link then Array.of_list (((t.cc :: t.cflags) @ [ c; "-o"; exe ]) @ t.ldflags @ libs)
        else Array.of_list ((t.cc :: t.cflags) @ [ "-fsyntax-only"; "-c"; c ])
      in
      Exec.ok cmd)

let header t h =
  cached t ("header " ^ h) (fun () ->
      let src = Printf.sprintf "#include <%s>\nint main(void){return 0;}\n" h in
      { ok = compiles t src; value = ""; note = "" })

let func t f lib =
  let libs = match lib with Some l -> [ "-l" ^ l ] | None -> [] in
  cached t (Printf.sprintf "func %s %s" f (String.concat "" libs)) (fun () ->
      let src =
        Printf.sprintf
          "char %s();\nint main(void){ return (int)(long)(void*)%s; }\n" f f
      in
      { ok = compiles t ~link:true ~libs src; value = ""; note = "" })

let symbol t s h =
  cached t (Printf.sprintf "symbol %s %s" s h) (fun () ->
      let src =
        Printf.sprintf
          "#include <%s>\nint main(void){\n#ifndef %s\n  (void) %s;\n#endif\n  return 0;\n}\n" h s s
      in
      { ok = compiles t src; value = ""; note = "" })

let sizes = [ 1; 2; 4; 8; 16; 3; 6; 12; 10; 24; 32; 5; 7; 9; 11; 14; 20; 64 ]

let sizeof t ty =
  cached t ("sizeof " ^ ty) (fun () ->
      let probe n =
        compiles t
          (Printf.sprintf "int meowc_probe[(sizeof(%s) == %d) ? 1 : -1];\n" ty n)
      in
      match List.find_opt probe sizes with
      | Some n -> { ok = true; value = string_of_int n; note = "" }
      | None -> { ok = false; value = ""; note = "no candidate size matched" })

let snippet t name src =
  cached t (Printf.sprintf "compiles %s %s" name (Digest.to_hex (Digest.string src)))
    (fun () -> { ok = compiles t ~link:true src; value = ""; note = "" })

let pkg_config t name constraint_ =
  let spec = match constraint_ with Some c -> name ^ " " ^ c | None -> name in
  cached t ("pkg " ^ spec) (fun () ->
      let args = String.split_on_char ' ' spec |> List.filter (fun s -> s <> "") in
      if not (Exec.ok (Array.of_list (("pkg-config" :: [ "--exists" ]) @ args))) then
        { ok = false; value = ""; note = "" }
      else
        let get flag =
          let code, out = Exec.capture (Array.of_list [ "pkg-config"; flag; name ]) in
          if code = 0 then String.trim out else ""
        in
        { ok = true; value = get "--cflags" ^ "\000" ^ get "--libs"; note = get "--modversion" })

let pkg_parts o =
  match String.index_opt o.value '\000' with
  | None -> ([], [])
  | Some i ->
      let split s = String.split_on_char ' ' s |> List.filter (fun x -> x <> "") in
      ( split (String.sub o.value 0 i),
        split (String.sub o.value (i + 1) (String.length o.value - i - 1)) )
