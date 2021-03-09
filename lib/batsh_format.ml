open Core_kernel
open Batsh_ast

let rec print_lvalue (buf : Buffer.t) (lvalue: leftvalue) =
  match lvalue with
  | Identifier ident ->
    Buffer.add_string buf ident
  | ListAccess (lvalue, expr) ->
    bprintf buf "%a[%a]" print_lvalue lvalue print_expression expr

and print_expression (buf : Buffer.t) (expr: expression) =
  match expr with
  | Leftvalue lvalue -> print_lvalue buf lvalue
  | Int number -> Buf