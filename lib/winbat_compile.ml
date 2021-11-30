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
     