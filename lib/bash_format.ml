
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