
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
        ~split_arith ~split_string ~symtable ~scope
    in
    Dlist.append assignments_left assignments_right, (left, right)
  in
  let split_when ~cond current_assignments new_expr =
    let split_expr_to_assignment assignments expr
      : (statement Dlist.t * expression) =
      if no_split_top then
        assignments, expr
      else
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
    split_when ~cond:split_primitive (Dlist.empty ()) expr
  | String _str ->
    split_when ~cond:split_string (Dlist.empty ()) expr
  | ArithUnary (operator, expr) ->
    let split = match operator with
      | "!" -> true
      | _ -> false
    in
    let assignments, expr = split_expression expr ~symtable ~scope
        ~split_arith:split
        ~split_string:true
    in
    split_when ~cond:split_arith assignments (ArithUnary (operator, expr))
  | ArithBinary (operator, left, right) ->
    let split = match operator with
      | "===" | "!==" | ">" | "<" | ">=" | "<=" -> true
      | _ -> false
    in
    let assignments, (left, right) = split_binary (left, right)
        ~split_arith:split
        ~split_string:true
    in
    split_when ~cond:split_arith
      assignments (ArithBinary (operator, left, right))
  | Concat (left, right) ->
    let assignments, (left, right) = split_binary (left, right)
        ~split_arith:true
        ~split_string:false