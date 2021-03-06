
open Core_kernel
open Batsh_ast

let rec split_expression
    ?(split_string = false)
    ?(split_list = true)
    ?(split_strcmp = true)
    (expr : expression)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : (statement Dlist.t * expression) =
  let split_binary ?(split_string = false) (left, right) =
    let assignments_left, left = split_expression left
        ~split_string ~symtable ~scope
    in
    let assignments_right, right = split_expression right
        ~split_string ~symtable ~scope
    in
    (Dlist.append assignments_left assignments_right), (left, right)
  in
  let split_when ~cond current_assignments new_expr =
    let split_expr_to_assignment assignments expr
      : (statement Dlist.t * expression) =
      let ident = Symbol_table.Scope.add_temporary_variable scope in
      let variable = Identifier ident in
      let assignments = Dlist.append
          assignments
          (Dlist.of_list [Assignment (variable, expr)])
      in
      assignments, (Leftvalue variable)
    in
    if cond then
      split_expr_to_assignment current_assignments new_expr
    else
      current_assignments, new_expr
  in
  match expr with
  | Bool _ | Int _ | Float _ | Leftvalue _ ->
    Dlist.empty () , expr
  | ArithUnary (operator, expr) ->
    let assignments, expr = split_expression expr
        ~split_string:true ~symtable ~scope
    in
    assignments, ArithUnary (operator, expr)
  | ArithBinary (operator, left, right) ->
    let assignments, (left, right) = split_binary (left, right)
        ~split_string:true
    in