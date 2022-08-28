
open Core_kernel
open Batsh_ast

let rec split_expression
    ?(no_split_top = false)
    ?(split_call = true)
    ?(split_arith = true)
    ?(split_string = false)
    ?(split_list = true)
    ?(split_primitive = false)
    (expr : expression)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : (statement Dlist.t * expression) =
  let split_binary (left, right) ~split_arith ~split_string =
    let assignments_left, left = split_expression left
        ~split_arith ~split_string ~symtable ~scope
    in
    let assignments_right, right = split_expression right