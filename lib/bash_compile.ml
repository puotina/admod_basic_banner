
open Core_kernel
open Bash_ast

module BAST = Batsh_ast

let is_arith (expr: BAST.expression) :bool =
  match expr with
  | BAST.String _
  | BAST.List _