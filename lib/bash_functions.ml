
open Core_kernel
open Bash_ast

let rec expand_leftvalue (lvalue : leftvalue) : leftvalue =
  match lvalue with
  | Identifier _ | EntireList _ | Cardinal _ ->
    lvalue
  | ListAccess (lvalue, arith) ->
    ListAccess (expand_leftvalue lvalue, arith)

let rec expand_expression (expr : expression) : expression =
  match expr with
  | Variable lvalue ->
    Variable (expand_leftvalue lvalue)
  | StrBinary (operator, left, right) ->
    StrBinary (operator, expand_expression left, expand_expression right)
  | Command (name, exprs) ->
    expand_command name exprs
  | List (exprs) ->
    List (expand_expressions exprs)
  | String _ | Result _ | Raw _ | TestUnary _ -> expr

and expand_expressions (exprs : expressions) : expressions =
  List.map exprs ~f: expand_expression

and expand_command (name : expression) (exprs : expressions) =
  let exprs = expand_expressions exprs in