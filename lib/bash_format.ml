
open Core_kernel
open Bash_ast

let rec print_lvalue_partial (buf : Buffer.t) (lvalue : leftvalue) =
  match lvalue with
  | Identifier ident ->
    Buffer.add_string buf ident
  | ListAccess (lvalue, arith) ->
    bprintf buf "%a[%a]"
      print_lvalue_partial lvalue
      (print_arith ~paren:false) arith
  | EntireList lvalue ->
    bprintf buf "%a[@]" print_lvalue_partial lvalue
  | Cardinal lvalue ->
    bprintf buf "#%a" print_lvalue_partial lvalue

and print_lvalue (buf : Buffer.t) (lvalue : leftvalue) ~(quote : bool) =
  let quote = if quote then "\"" else "" in
  match lvalue with
  | Identifier _ident ->
    bprintf buf "%s$%a%s" quote print_lvalue_partial lvalue quote
  | ListAccess _
  | EntireList _
  | Cardinal _ ->
    bprintf buf "%s${%a}%s" quote print_lvalue_partial lvalue quote

and print_arith
    ?(paren = true)
    (buf : Buffer.t)
    (expr: arithmetic)
  =
  match expr with
  | Leftvalue lvalue -> print_lvalue buf lvalue ~quote:false
  | Int number -> Buffer.add_string buf (string_of_int number)
  | Float number -> Buffer.add_string buf (Float.to_string number)
  | ArithUnary (operator, arith) ->
    if paren then
      bprintf buf "%s(%a)" operator (print_arith ~paren:true) arith
    else
      bprintf buf "%s%a" operator (print_arith ~paren:true) arith
  | ArithBinary binary ->
    if paren then
      bprintf buf "(%a)" print_arith_binary binary
    else
      print_arith_binary buf binary

and print_arith_binary
    (buf : Buffer.t)
    (operator, left, right)
  =
  let operator = match operator with
    | "===" -> "=="
    | "!==" -> "!="
    | _ -> operator
  in
  bprintf buf "%a %s %a"
    (print_arith ~paren:true) left
    operator
    (print_arith ~paren:true) right

let rec print_expression buf (expr: expression) =
  match expr with
  | Variable lvalue | Result Leftvalue lvalue ->
    print_lvalue buf lvalue ~quote:true
  | String str ->
    bprintf buf "\"%s\"" (Formatutil.escape str)
  | Result arith ->
    bprintf buf "$((%a))" (print_arith ~paren:false) arith
  | StrBinary binary ->
    print_str_binary buf binary
  | TestUnary test ->
    print_test_unary buf test
  | Command cmd ->
    bprintf buf "$(%a)" print_command cmd
  | List exprs ->
    Buffer.add_string buf "(";
    let num_exprs = List.length exprs in
    List.iteri exprs ~f: (fun i expr ->
        print_expression buf expr;
        if i <> num_exprs - 1 then
          Buffer.add_string buf " "
      );
    Buffer.add_string buf ")"