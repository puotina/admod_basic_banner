open Core_kernel
open Batsh_ast
open Winbat_ast

let rec compile_leftvalue
    (lvalue: Batsh_ast.leftvalue)
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.Scope.t)
  : leftvalue =
  match lvalue with
  | Identifier ident ->
    `Identifier ident
  | ListAccess (lvalue, index) ->
    let lvalu