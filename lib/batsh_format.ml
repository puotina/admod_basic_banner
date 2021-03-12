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
  | Int number -> Buffer.add_string buf (string_of_int number)
  | Float number -> Buffer.add_string buf (Float.to_string number)
  | String str -> bprintf buf "\"%s\"" (Formatutil.escape str)
  | Bool true  -> Buffer.add_string buf "true"
  | Bool false -> Buffer.add_string buf "false"
  | ArithUnary (operator, expr) ->
    bprintf buf "%s(%a)" operator print_expression expr
  | ArithBinary binary | StrCompare binary ->
    print_binary_expression buf binary
  | Concat (left, right) ->
    print_binary_expression buf ("++", left, right)
  | Call (ident, exprs) ->
    bprintf buf "%s(%a)" ident print_expressions exprs
  | List exprs ->
    bprintf buf "[%a]" print_expressions exprs

and print_expressions (buf : B