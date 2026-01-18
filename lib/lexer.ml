type kind =
  | Word of string
  | Str of string
  | Lbrace
  | Rbrace
  | Lparen
  | Rparen
  | Newline
  | Eof

type t = { kind : kind; span : Span.t }

let describe = function
  | Word w -> Printf.sprintf "%S" w
  | Str _ -> "a string"
  | Lbrace -> "'{'"
  | Rbrace -> "'}'"
  | Lparen -> "'('"
  | Rparen -> "')'"
  | Newline -> "end of line"
  | Eof -> "end of file"

let is_space c = c = ' ' || c = '\t' || c = '\r'
let is_delim c = is_space c || c = '\n' || c = '{' || c = '}' || c = '(' || c = ')' || c = '#'

let escape = function
  | 'n' -> '\n'
  | 't' -> '\t'
  | 'r' -> '\r'
  | '0' -> '\000'
  | c -> c

let tokens ~file src =
  let n = String.length src in
  let out = ref [] in
  let line = ref 1 and bol = ref 0 in
  let i = ref 0 in
  let col () = !i - !bol + 1 in
  let emit kind start_col len = out := { kind; span = Span.make file !line start_col len } :: !out in
  while !i < n do
    let c = src.[!i] in
    let c0 = col () in
    if is_space c then incr i
    else if c = '#' then while !i < n && src.[!i] <> '\n' do incr i done
    else if c = '\\' && !i + 1 < n && src.[!i + 1] = '\n' then begin
      i := !i + 2;
      incr line;
      bol := !i
    end
    else if c = '\n' then begin
      emit Newline c0 1;
      incr i;
      incr line;
      bol := !i
    end
    else if c = '{' then (emit Lbrace c0 1; incr i)
    else if c = '}' then (emit Rbrace c0 1; incr i)
    else if c = '(' then (emit Lparen c0 1; incr i)
    else if c = ')' then (emit Rparen c0 1; incr i)
    else if c = '"' then begin
      let buf = Buffer.create 16 in
      let start_line = !line in
      incr i;
      let closed = ref false in
      while !i < n && not !closed do
        if src.[!i] = '"' then (closed := true; incr i)
        else if src.[!i] = '\\' && !i + 1 < n then begin
          Buffer.add_char buf (escape src.[!i + 1]);
          i := !i + 2
        end
        else begin
          if src.[!i] = '\n' then (incr line; bol := !i + 1);
          Buffer.add_char buf src.[!i];
          incr i
        end
      done;
      if not !closed then
        Diag.error ~span:(Span.make file start_line c0 1) "unterminated string";
      emit (Str (Buffer.contents buf)) c0 (max 1 (col () - c0))
    end
    else begin
      let start = !i in
      let fin = ref false in
      while !i < n && not !fin do
        if src.[!i] = '$' && !i + 1 < n && src.[!i + 1] = '{' then begin
          i := !i + 2;
          while !i < n && src.[!i] <> '}' && src.[!i] <> '\n' do incr i done;
          if !i < n && src.[!i] = '}' then incr i
          else Diag.error ~span:(Span.make file !line c0 (!i - start)) "unclosed ${...}"
        end
        else if is_delim src.[!i] then fin := true
        else incr i
      done;
      emit (Word (String.sub src start (!i - start))) c0 (!i - start)
    end
  done;
  emit Eof (col ()) 1;
  List.rev !out
