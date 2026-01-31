open Ast

type p = { toks : Lexer.t array; mutable pos : int }

let stmt_keywords =
  [ "project"; "set"; "append"; "option"; "include"; "subdir"; "if"; "check"; "pkg";
    "config_header"; "message" ]

let block_kinds = [ "toolchain"; "lib"; "shared"; "bin"; "test"; "rule"; "install" ]
let check_kinds = [ "header"; "func"; "symbol"; "sizeof"; "compiles" ]

let peek p = p.toks.(p.pos)
let kind p = (peek p).Lexer.kind
let span p = (peek p).Lexer.span
let advance p = if p.pos < Array.length p.toks - 1 then p.pos <- p.pos + 1

let bump p =
  let t = peek p in
  advance p;
  t

let rec skip_nl p = match kind p with Lexer.Newline -> advance p; skip_nl p | _ -> ()

let expected p what =
  Diag.error ~span:(span p) "expected %s, found %s" what (Lexer.describe (kind p))

let word_value p =
  match kind p with
  | Lexer.Word w -> let s = span p in advance p; { text = w; span = s; quoted = false }
  | Lexer.Str w -> let s = span p in advance p; { text = w; span = s; quoted = true }
  | _ -> expected p "a value"

let is_value p = match kind p with Lexer.Word _ | Lexer.Str _ -> true | _ -> false

let name_value p what =
  match kind p with
  | Lexer.Word w -> let s = span p in advance p; { text = w; span = s; quoted = false }
  | _ -> expected p what

let eat_lbrace p =
  skip_nl p;
  match kind p with Lexer.Lbrace -> advance p | _ -> expected p "'{'"

let end_of_line p =
  match kind p with
  | Lexer.Newline | Lexer.Eof -> ()
  | Lexer.Rbrace -> ()
  | _ -> expected p "end of line"

let values_until_eol p =
  let acc = ref [] in
  while is_value p do acc := word_value p :: !acc done;
  end_of_line p;
  List.rev !acc

let at_word p w = match kind p with Lexer.Word x -> x = w | _ -> false

(* conditions: or > and > cmp > not > primary *)
let rec expr p = or_expr p

and or_expr p =
  let l = ref (and_expr p) in
  while at_word p "or" do advance p; l := Or (!l, and_expr p) done;
  !l

and and_expr p =
  let l = ref (cmp_expr p) in
  while at_word p "and" do advance p; l := And (!l, cmp_expr p) done;
  !l

and cmp_expr p =
  let l = unary p in
  match kind p with
  | Lexer.Word (("==" | "!=") as op) -> (
      advance p;
      let r = word_value p in
      match l with
      | Atom a -> Cmp (a, op, r)
      | _ -> Diag.error ~span:r.span "%s compares values, not conditions" op)
  | _ -> l

and unary p =
  if at_word p "not" then (let s = span p in advance p; Not (unary p, s)) else primary p

and primary p =
  match kind p with
  | Lexer.Lparen ->
      advance p;
      let e = expr p in
      (match kind p with Lexer.Rparen -> advance p | _ -> expected p "')'");
      e
  | Lexer.Word _ | Lexer.Str _ -> Atom (word_value p)
  | _ -> expected p "a condition"

let rec fitems p =
  let acc = ref [] in
  let stop = ref false in
  while not !stop do
    skip_nl p;
    match kind p with
    | Lexer.Rbrace -> advance p; stop := true
    | Lexer.Eof -> Diag.error ~span:(span p) "unclosed block"
    | Lexer.Word "if" -> let a, b = fif p fitems in acc := FIf (a, b) :: !acc
    | Lexer.Word key ->
        let kspan = span p in
        advance p;
        let values = values_until_eol p in
        if values = [] then
          Diag.error ~span:kspan ~hint:"write a value after the field name"
            "field %S has no value" key;
        acc := FField { key; kspan; values } :: !acc
    | _ -> expected p "a field name or '}'"
  done;
  List.rev !acc

and fif : 'a. p -> (p -> 'a list) -> (expr * 'a list) list * 'a list =
  fun p body ->
   advance p;
   let cond = expr p in
   eat_lbrace p;
   let taken = body p in
   let arms = ref [ (cond, taken) ] in
   let fallback = ref [] in
   let stop = ref false in
   while not !stop do
     let save = p.pos in
     skip_nl p;
     if at_word p "else" then begin
       advance p;
       if at_word p "if" then begin
         advance p;
         let c = expr p in
         eat_lbrace p;
         arms := (c, body p) :: !arms
       end
       else begin
         eat_lbrace p;
         fallback := body p;
         stop := true
       end
     end
     else (p.pos <- save; stop := true)
   done;
   (List.rev !arms, !fallback)

let check_stmt p kspan =
  let what = name_value p "a check kind" in
  let c =
    match what.text with
    | "header" -> Header (word_value p)
    | "sizeof" -> Sizeof (word_value p)
    | "func" ->
        let f = word_value p in
        if at_word p "in" then (advance p; Func (f, Some (word_value p))) else Func (f, None)
    | "symbol" ->
        let s = word_value p in
        if not (at_word p "in") then
          Diag.error ~span:(span p) ~hint:"write: check symbol NAME in HEADER" "expected 'in'";
        advance p;
        Symbol (s, word_value p)
    | "compiles" ->
        let n = word_value p in
        let body = word_value p in
        if not body.quoted then
          Diag.error ~span:body.span "the program for 'check compiles' must be a quoted string";
        Compiles (n, body)
    | other ->
        Diag.error ~span:what.span ~hint:(Suggest.hint other check_kinds) "unknown check %S" other
  in
  end_of_line p;
  Check (c, kspan)

let rec stmt p =
  let t = peek p in
  match t.Lexer.kind with
  | Lexer.Word "project" ->
      advance p;
      let name = name_value p "the project name" in
      end_of_line p;
      Project name
  | Lexer.Word "set" ->
      advance p;
      let n = name_value p "a variable name" in
      Set (n, values_until_eol p)
  | Lexer.Word "append" ->
      advance p;
      let n = name_value p "a variable name" in
      Append (n, values_until_eol p)
  | Lexer.Word "option" ->
      advance p;
      let n = name_value p "an option name" in
      let ty = name_value p "the option type (bool, string or path)" in
      if not (List.mem ty.text [ "bool"; "string"; "path" ]) then
        Diag.error ~span:ty.span ~hint:(Suggest.hint ty.text [ "bool"; "string"; "path" ])
          "unknown option type %S" ty.text;
      Option (n, ty, values_until_eol p)
  | Lexer.Word "include" -> advance p; let v = word_value p in end_of_line p; Include v
  | Lexer.Word "subdir" -> advance p; let v = word_value p in end_of_line p; Subdir v
  | Lexer.Word "config_header" -> advance p; let v = word_value p in end_of_line p; ConfigHeader v
  | Lexer.Word "message" ->
      advance p;
      let lvl = name_value p "a message level (info, warn or error)" in
      if not (List.mem lvl.text [ "info"; "warn"; "error" ]) then
        Diag.error ~span:lvl.span ~hint:(Suggest.hint lvl.text [ "info"; "warn"; "error" ])
          "unknown message level %S" lvl.text;
      Message (lvl, values_until_eol p)
  | Lexer.Word "check" -> advance p; check_stmt p t.Lexer.span
  | Lexer.Word "pkg" ->
      advance p;
      let n = word_value p in
      let c = if is_value p then Some (word_value p) else None in
      end_of_line p;
      Pkg (n, c, t.Lexer.span)
  | Lexer.Word "if" -> let arms, els = fif p stmts_body in If (arms, els)
  | Lexer.Word k when List.mem k block_kinds ->
      advance p;
      let bname, nspan =
        if k = "toolchain" then ("", t.Lexer.span)
        else
          let v = name_value p (Printf.sprintf "a name for this %s" k) in
          (v.text, v.span)
      in
      eat_lbrace p;
      Blk { kind = k; kspan = t.Lexer.span; bname; nspan; items = fitems p }
  | Lexer.Word other ->
      Diag.error ~span:t.Lexer.span
        ~hint:(Suggest.hint other (stmt_keywords @ block_kinds))
        "unknown declaration %S" other
  | k -> Diag.error ~span:t.Lexer.span "expected a declaration, found %s" (Lexer.describe k)

and stmts_body p =
  let acc = ref [] in
  let stop = ref false in
  while not !stop do
    skip_nl p;
    match kind p with
    | Lexer.Rbrace -> advance p; stop := true
    | Lexer.Eof -> Diag.error ~span:(span p) "unclosed block"
    | _ -> acc := stmt p :: !acc
  done;
  List.rev !acc

let program p =
  let acc = ref [] in
  let stop = ref false in
  while not !stop do
    skip_nl p;
    match kind p with Lexer.Eof -> stop := true | _ -> acc := stmt p :: !acc
  done;
  List.rev !acc

let string ~file src =
  Diag.register file src;
  let toks = Array.of_list (Lexer.tokens ~file src) in
  program { toks; pos = 0 }

let file path =
  match Fs.read path with
  | exception Sys_error m -> Diag.error "%s" m
  | src -> string ~file:path src
