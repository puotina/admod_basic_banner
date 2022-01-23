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
    let lvalue = compile_leftvalue lvalue ~symtable ~scope in
    let index = compile_expression_to_varint index ~symtable ~scope in
    `ListAccess (lvalue, index)

and compile_expression_to_varint
    (expr : Batsh_ast.expression)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : varint =
  match expr with
  | Leftvalue lvalue ->
    `Var (compile_leftvalue lvalue ~symtable ~scope)
  | Int num ->
    `Int num
  | _ ->
    raise (Errors.SemanticError
             ("Index should be either var or int",
              expr |> Batsh_ast.sexp_of_expression |> Sexp.to_string
             )
          )

let rec compile_expression_to_arith
    (expr : Batsh_ast.expression)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : arithmetic =
  match expr with
  | Bool false ->
    `Int 0
  | Bool true ->
    `Int 1
  | Int num ->
    `Int num
  | Leftvalue lvalue ->
    `Var (compile_leftvalue lvalue ~symtable ~scope)
  | ArithUnary (operator, expr) ->
    `ArithUnary (operator, compile_expression_to_arith expr ~symtable ~scope)
  | ArithBinary (operator, left, right) ->
    `ArithBinary (operator,
                  compile_expression_to_arith left ~symtable ~scope,
                  compile_expression_to_arith right ~symtable ~scope)
  | String _
  | Float _
  | List _
  | Concat _
  | StrCompare _
  | Call _ ->
    Sexp.output_hum stderr (Batsh_ast.sexp_of_expression expr);
    failwith "Can not be here"

let compile_expression
    (expr : Batsh_ast.expression)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : varstrings =
  let rec compile_expression_impl
      (expr : Batsh_ast.expression)
    : varstring Dlist.t =
    match expr with
    | Bool false ->
      Dlist.of_list [`Str "0"]
    | Bool true ->
      Dlist.of_list [`Str "1"]
    | Int num ->
      Dlist.of_list [`Str (string_of_int num)]
    | Float num ->
      Dlist.of_list [`Str (Float.to_string num)]
    | String str ->
      Dlist.of_list [`Str str]
    | Leftvalue lvalue ->
      Dlist.of_list [`Var (compile_leftvalue lvalue ~symtable ~scope)]
    | Concat (left, right) ->
      let left = compile_expression_impl left in
      let right = compile_expression_impl right in
      Dlist.append left right
    | List _
    | ArithUnary _
    | ArithBinary _
    | StrCompare _
    | Call _ ->
      Sexp.output_hum stderr (Batsh_ast.sexp_of_expression expr);
      failwith "Bug: Must have been split into assignments."
  in
  Dlist.to_list (compile_expression_impl expr)

let compile_expressions_to_arguments
    (exprs : Batsh_ast.expressions)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : parameters =
  List.map exprs ~f: (compile_expression ~symtable ~scope)

let compile_expression_to_comparison
    (expr : Batsh_ast.expression)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : comparison =
  match expr with
  | ArithUnary (operator, sub_expr) ->
    let sub_expr = compile_expression sub_expr ~symtable ~scope in
    `UniCompare (operator, sub_expr)
  | StrCompare (operator, left, right)
  | ArithBinary (operator, left, right) ->
    let left = compile_expression left ~symtable ~scope in
    let right = compile_expression right ~symtable ~scope in
    `StrCompare (operator, left, right)
  | Leftvalue lvalue ->
    let lvalue = `Var (compile_leftvalue lvalue ~symtable ~scope) in
    `StrCompare ("==", [lvalue], [`Str "1"])
  | Bool true | Int 1 ->
    `UniCompare ("", [`Str "1"])
  | Bool false | Int _ ->
    `UniCompare ("!", [`Str "1"])
  | Call ("exists", (sub_expr :: _)) ->
    let clause = compile_expression sub_expr ~symtable ~scope in
    `TestCompare ("exist", clause)
  | _ ->
    raise (Errors.SemanticError
             ("Expression can not compile to comparison",
              expr |> Batsh_ast.sexp_of_expression |> Sexp.to_string
             )
          )

let compile_call
    (ident, exprs)
    ~(return_value : leftvalue option)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : statements =
  let args = compile_expressions_to_arguments exprs ~symtable ~scope in
  if Symbol_table.is_function symtable ident then
    (* function call *)
    let frame_pointer_assign, frame_pointer =
      if Symbol_table.Scope.is_function scope then
        (* add frame pointer as surffix to local variables *)
        (* increase frame pointer %~2 by 1 *)
        let frame_pointer = `Identifier (
            Symbol_table.Scope.add_temporary_variable scope)
        in
        [`ArithAssign (
            frame_pointer,
            `ArithBinary ("+", `Int 1, `Var (`Identifier "%~2"))
          )
        ], `Var (frame_pointer)
      else
        (* call from toplevel *)
        [], `Str "0"
    in
    let retval = Symbol_table.Scope.add_temporary_variable scope in
    let surffix =
      if Symbol_table.Scope.is_function scope then
        (* call from function scope *)
        "_%~2"
      else
        (* call from toplevel *)
        ""
    in
    let surffixed_retval = `Rawstr (retval ^ surffix) in
    let stringified_args = List.map args ~f:(fun arg ->
        match arg with
        | [`Var (`Identifier ident)] ->
          [`Rawstr (ident ^ surffix)]
        | _ ->
          Sexp.output_hum stderr (Winbat_ast.