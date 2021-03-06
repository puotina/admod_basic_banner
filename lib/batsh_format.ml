open Core_kernel
open Batsh_ast

let rec print_lvalue (buf : Buffer.t) (lvalue: leftvalue) =
  matc