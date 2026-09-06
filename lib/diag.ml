type severity = Error | Warning | Note

type t = {
  severity : severity;
  span : Span.t;
  msg : string;
  hint : string option;
  notes : (Span.t * string) list;
}

exception Stop of t list

let make ?(severity = Error) ?(span = Span.none) ?hint ?(notes = []) msg =
  { severity; span; msg; hint; notes }

let error ?span ?hint ?notes fmt =
  Printf.ksprintf (fun msg -> raise (Stop [ make ?span ?hint ?notes msg ])) fmt


let sources : (string, string array) Hashtbl.t = Hashtbl.create 8

let register file text = Hashtbl.replace sources file (Array.of_list (String.split_on_char '\n' text))

let line_of file n =
  match Hashtbl.find_opt sources file with
  | Some lines when n >= 1 && n <= Array.length lines -> Some lines.(n - 1)
  | _ -> None

let tag = function
  | Error -> Style.red "error"
  | Warning -> Style.yellow "warning"
  | Note -> Style.blue "note"

let gutter n = Style.dim (Style.pad_left 5 n ^ " " ^ Style.dim "\xe2\x94\x82")

let caret_line span text =
  let col = max 1 span.Span.col in
  let len = max 1 span.Span.len in
  let lead = String.make (min (String.length text + 1) (col - 1)) ' ' in
  lead ^ String.make len '^'

let render_span buf severity (span : Span.t) =
  match line_of span.file span.line with
  | None -> ()
  | Some text ->
      let paint = match severity with Error -> Style.red | Warning -> Style.yellow | Note -> Style.blue in
      Buffer.add_string buf (Printf.sprintf "%s\n" (gutter ""));
      Buffer.add_string buf (Printf.sprintf "%s %s\n" (gutter (string_of_int span.line)) text);
      Buffer.add_string buf (Printf.sprintf "%s %s\n" (gutter "") (paint (caret_line span text)))

let render d =
  let buf = Buffer.create 256 in
  let loc = Span.to_string d.span in
  Buffer.add_string buf
    (Printf.sprintf "%s%s %s\n"
       (if loc = "" then "" else Style.bold loc ^ " ")
       (tag d.severity) d.msg);
  render_span buf d.severity d.span;
  List.iter
    (fun (span, note) ->
      Buffer.add_string buf (Printf.sprintf "%s %s %s\n" (gutter "") (Style.dim "note:") note);
      render_span buf Note span)
    d.notes;
  (match d.hint with
  | Some h -> Buffer.add_string buf (Printf.sprintf "%s %s %s\n" (gutter "") (Style.dim "=") h)
  | None -> ());
  Buffer.contents buf

let print ds = List.iter (fun d -> prerr_string (render d)) ds
