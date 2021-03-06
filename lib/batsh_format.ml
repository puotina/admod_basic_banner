open Core_kernel
open Batsh_ast

let rec print_lvalue (buf : Buffer.t) (lvalue: leftvalue) =
  match lvalue with
  | Identifier ident ->
    Buffer.add_string buf ident
  | ListAccess (lvalue, expr) ->
    bprintf buf