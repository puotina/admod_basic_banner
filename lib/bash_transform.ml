
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
    assignments, ArithBinary (operator, left, right)
  | String str ->
    split_when ~cond:split_string (Dlist.empty ()) (String str)
  | Concat (left, right) ->
    let assignments, (left, right) = split_binary (left, right) in
    split_when ~cond:split_string assignments (Concat (left, right))
  | StrCompare (operator, left, right) ->
    let assignments, (left, right) = split_binary (left, right) in
    split_when ~cond:split_strcmp assignments (StrCompare (operator, left, right))
  | Call (ident, exprs) ->
    let assignments, exprs = split_expressions exprs ~symtable ~scope in
    split_when ~cond:split_string assignments (Call (ident, exprs))
  | List exprs ->
    let assignments, exprs = split_expressions exprs ~symtable ~scope in
    split_when ~cond:split_list assignments (List exprs)

and split_expressions
    (exprs : expressions)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : (statement Dlist.t * expressions) =
  let assignments, exprs = List.fold exprs ~init: (Dlist.empty (), [])
      ~f: (fun (assignments_acc, exprs_acc) expr ->
          let assignments, expr = split_expression expr ~symtable ~scope in
          (Dlist.append assignments assignments_acc, expr :: exprs_acc)
        )
  in
  assignments, List.rev exprs

let rec split_statement
    (stmt : statement)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : statement =
  let prepend_assignments (assignments : statement Dlist.t) stmt : statement =
    if Dlist.length assignments = 0 then
      stmt
    else
      Block (Dlist.to_list (Dlist.append assignments (Dlist.of_list [stmt])))
  in
  match stmt with
  | Empty | Global _ | Comment _ | Return None ->
    stmt
  | Expression expr ->
    let assignments, expr = split_expression expr
        ~split_list:false ~symtable ~scope
    in
    prepend_assignments assignments (Expression expr)
  | Assignment (lvalue, expr) ->
    let assignments, expr = split_expression expr
        ~split_list:false ~split_strcmp:false ~symtable ~scope