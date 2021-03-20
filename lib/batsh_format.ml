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

and print_expressions (buf : Buffer.t) (exprs: expression list) =
  Formatutil.print_separate_list buf exprs
    ~f: print_expression ~separator: ", "

and print_binary_expression
    (buf : Buffer.t)
    (operator, left, right)
  =
  bprintf buf "(%a %s %a)"
    print_expression left operator print_expression right

let rec print_statement (buf : Buffer.t) (stmt: statement) ~(indent: int) =
  let () = match stmt with
    | Block _ -> ()
    | _ ->
      Formatutil.print_indent buf indent in
  match stmt with
  | Comment comment ->
    bprintf buf "//%s" comment
  | Block inner_stmts -> print_block_statement ~indent buf inner_stmts
  | Expression expr ->
    print_expression buf expr;
    Buffer.add_string buf ";"
  | Assignment (lvalue, expr) ->
    bprintf buf "%a = %a;" print_lvalue lvalue print_expression expr
  | If (expr, stmt) ->
    print_if_statement buf expr stmt ~in