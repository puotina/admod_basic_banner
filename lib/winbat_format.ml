
open Core_kernel
open Winbat_ast

let escape (str : string) : string =
  let buffer = Buffer.create (String.length str) in
  let exclamation = match String.index str '!' with
    | None -> false
    | Some _ -> true
  in
  String.iter str ~f:(fun ch ->
      let escaped = match ch with
        | '%' -> "%%"
        | '^' ->
          if exclamation then
            "^^^^"
          else
            "^^"
        | '&' -> "^&"
        | '<' -> "^<"
        | '>' -> "^>"
        | '\'' -> "^'"
        | '"' -> "^\""
        | '`' -> "^`"
        | ',' -> "^,"
        | ';' -> "^;"
        | '=' -> "^="
        | '(' -> "^("
        | ')' -> "^)"
        | '!' -> "^^!"
        | '\n' -> "^\n\n"
        | _ -> String.of_char ch
      in
      Buffer.add_string buffer escaped
    );
  Buffer.contents buffer

let rec print_leftvalue
    (buf : Buffer.t)
    (lvalue : leftvalue)
    ~(bare : bool)
  =
  match lvalue with
  | `Identifier ident ->
    if bare || (Char.equal (String.get ident 0) '%') then
      bprintf buf "%s" ident
    else
      bprintf buf "!%s!" ident
  | `ListAccess (lvalue, index) ->
    if bare then
      bprintf buf "%a_%a"
        (print_leftvalue ~bare: true) lvalue
        (print_varint ~bare: true) index
    else
      bprintf buf "!%a_%a!"
        (print_leftvalue ~bare: true) lvalue
        (print_varint ~bare: true) index

and print_varint
    (buf : Buffer.t)
    (index : varint)
    ~(bare : bool)