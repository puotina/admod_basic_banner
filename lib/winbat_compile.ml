open Core_kernel
open Batsh_ast
open Winbat_ast

let rec compile_leftvalue
    (lvalue: Batsh_ast.leftvalue)
    ~(symtable: Symbol_table.t)
    ~(scop