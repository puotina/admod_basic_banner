open Core_kernel
open Batsh_ast
open Winbat_ast

let rec compile_leftvalue
    (lvalue: Batsh_as