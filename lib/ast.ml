type value = { text : string; span : Span.t; quoted : bool }

type expr =
  | Atom of value
  | Not of expr * Span.t
  | And of expr * expr
  | Or of expr * expr
  | Cmp of value * string * value

type field = { key : string; kspan : Span.t; values : value list }

type fitem =
  | FField of field
  | FIf of (expr * fitem list) list * fitem list

type block = {
  kind : string;
  kspan : Span.t;
  bname : string;
  nspan : Span.t;
  items : fitem list;
}

type check =
  | Header of value
  | Func of value * value option
  | Symbol of value * value
  | Sizeof of value
  | Compiles of value * value

type stmt =
  | Project of value
  | Set of value * value list
  | Append of value * value list
  | Option of value * value * value list
  | Include of value
  | Subdir of value
  | If of (expr * stmt list) list * stmt list
  | Check of check * Span.t
  | Pkg of value * value option * Span.t
  | ConfigHeader of value
  | Message of value * value list
  | Blk of block

let v text span = { text; span; quoted = false }
