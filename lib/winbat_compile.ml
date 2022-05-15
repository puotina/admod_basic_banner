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
          Sexp.output_hum stderr (Winbat_ast.sexp_of_parameter arg);
          failwith "Argument should have been converted to variable."
      )
    in
    let prefixed_args = [
      [surffixed_retval]; (* return value *)
      [frame_pointer]; (* frame pointer *)
    ] @ stringified_args in
    let call_stmt = `Call ([`Str ("call :" ^ ident)], prefixed_args) in
    let stmts = frame_pointer_assign @ [call_stmt] in
    let stmts =
      match return_value with
      | Some lvalue ->
        (* Assign return value *)
        stmts @ [`Assignment (lvalue, [`Var (`Identifier retval)])]
      | None ->
        (* Print out return value *)
        stmts @ [`Call ([`Str "print"], [[`Var (`Identifier retval)]])]
    in
    stmts
  else
    match ident with
    | "exists" ->
      let params_1 params =
        match params with
        | param :: _ -> param
        | _ -> failwith ("exists must have only 1 parameter.")
      in
      let arg = compile_expression (params_1 exprs) ~symtable ~scope in
      let cond = `TestCompare ("exist", arg) in
      let stmts =
        match return_value with
        | Some lvalue ->
          let true_stmt = [`ArithAssign (lvalue, `Int 1)] in
          let false_stmt = [`ArithAssign (lvalue, `Int 0)] in
          [`IfElse (cond, true_stmt, false_stmt)]
        | None ->
          [`IfElse (cond, [], [])]
      in
      stmts
    | _ ->
      (* external command *)
      let stmts =
        match return_value with
        | Some lvalue ->
          [`Output (lvalue, [`Str ident], args)]
        | None ->
          [`Call ([`Str ident], args)]
      in
      stmts

let compile_expression_statement
    (expr : Batsh_ast.expression)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : statements =
  match expr with
  | Call call ->
    (* Call discarding return value *)
    compile_call call ~return_value:None ~symtable ~scope
  | Leftvalue _ ->
    [] (* No side effect *)
  | _ ->
    Sexp.output_hum stderr (Batsh_ast.sexp_of_expression expr);
    assert false (* TODO *)

let compile_arith_assignment
    (lvalue : Batsh_ast.leftvalue)
    (expr : Batsh_ast.expression)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : statements =
  match expr with
  | ArithBinary ("===", _, _)
  | ArithBinary ("!==", _, _)
  | ArithBinary (">", _, _)
  | ArithBinary ("<", _, _)
  | ArithBinary (">=", _, _)
  | ArithBinary ("<=", _, _)
  | ArithUnary ("!", _) ->
    let cond = compile_expression_to_comparison expr ~symtable ~scope in
    let lvalue = compile_leftvalue lvalue ~symtable ~scope in
    let true_stmt = [`ArithAssign (lvalue, `Int 1)] in
    let false_stmt = [`ArithAssign (lvalue, `Int 0)] in
    [`IfElse (cond, true_stmt, false_stmt)]
  | Bool _
  | Int _
  | Float _
  | ArithUnary _
  | ArithBinary _ ->
    let lvalue = compile_leftvalue lvalue ~symtable ~scope in
    let arith = compile_expression_to_arith expr ~symtable ~scope in
    [`ArithAssign (lvalue, arith)]
  | _ ->
    Sexp.output_hum stderr (Batsh_ast.sexp_of_leftvalue lvalue);
    Sexp.output_hum stderr (Batsh_ast.sexp_of_expression expr);
    failwith "Can not reach here."

let rec compile_statement
    (stmt : Batsh_ast.statement)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : statements =
  match stmt with
  | Comment comment ->
    [`Comment comment]
  | Block stmts ->
    compile_statements stmts ~symtable ~scope
  | Expression expr ->
    compile_expression_statement expr ~symtable ~scope
  | Assignment (lvalue, expr) ->
    compile_assignment lvalue expr ~symtable ~scope
  | If (expr, stmt) ->
    [`If (compile_expression_to_comparison expr ~symtable ~scope,
          compile_statement stmt ~symtable ~scope)]
  | IfElse (expr, then_stmt, else_stmt) ->
    [`IfElse (compile_expression_to_comparison expr ~symtable ~scope,
              compile_statement then_stmt ~symtable ~scope,
              compile_statement else_stmt ~symtable ~scope)]
  | While (expr, stmt) ->
    let condition = compile_expression_to_comparison expr ~symtable ~scope in
    let body = compile_statement stmt ~symtable ~scope in
    let label_surfix = Symbol_table.Scope.add_temporary_variable
        (Symbol_table.global_scope symtable)
    in
    let label = sprintf "WHILE%s" label_surfix in
    [
      `Label label;
      `If (condition, body @ [`Goto label]);
    ]
  | Return (Some expr) ->
    [
      `Assignment (`Identifier "%~1", compile_expression expr ~symtable ~scope);
      `Goto ":EOF"
    ]
  | Return None ->
    [`Goto ":EOF"]
  | Global _
  | Empty ->
    []

and compile_assignment
    (lvalue : Batsh_ast.leftvalue)
    (expr : Batsh_ast.expression)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : statements =
  match expr with
  | String _
  | Concat _
  | Leftvalue _ ->
    let lvalue = compile_leftvalue lvalue ~symtable ~scope in
    [`Assignment (lvalue, compile_expression expr ~symtable ~scope)]
  | Bool _
  | Int _
  | Float _
  | ArithUnary _
  | ArithBinary _ ->
    compile_arith_assignment lvalue expr ~symtable ~scope
  | List exprs ->
    List.concat (List.mapi exprs ~f: (fun i expr ->
        compile_assignment (ListAccess (lvalue, (Int i))) expr ~symtable ~scope
      ))
  | Call call ->
    (* Call obtaining return value *)
    let lvalue = compile_leftvalue lvalue ~symtable ~scope in
    compile_call call ~return_value:(Some lvalue) ~symtable ~scope
  | StrCompare _ ->
    let comp = compile_expression_to_comparison expr ~symtable ~scope in
    let lvalue = compile_leftvalue lvalue ~symtable ~scope in
    [`IfElse (
        comp,
        [`ArithAssign (lvalue, `Int 1)],
        [`ArithAssign (lvalue, `Int 0)]
      )
    ]

and compile_statements
    (stmts: Batsh_ast.statements)
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.Scope.t)
  : statements =
  Dlist.to_list (
    List.fold stmts ~init: (Dlist.empty ()) ~f: (fun acc stmt ->
        let stmts = compile_statement stmt ~symtable ~scope in
        Dlist.append acc (Dlist.of_list stmts)
      )
  )

(* Function variable, call, return replacement *)

let rec compile_function_leftvalue
    (lvalue : leftvalue)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : leftvalue =
  match lvalue with
  | `Identifier ident ->
    if Symbol_table.Scope.is_global_variable scope ~name: ident then
      lvalue
    else
      (* Add surfix _%~2 to local variable *)
      `ListAccess (lvalue, `Var (`Identifier "%~2"))
  | `ListAccess (lvalue, index) ->
    `ListAccess (compile_function_leftvalue lvalue ~symtable ~scope, index)

let compile_function_varstring
    (var : varstring)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : varstring =
  match var with
  | `Var lvalue ->
    `Var (compile_function_leftvalue lvalue ~symtable ~scope)
  | `Str _ | `Rawstr _ ->
    var

let compile_function_varstrings
    (vars : var