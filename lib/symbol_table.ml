open Core_kernel
open Batsh_ast

type variable_entry = {
  name : string;
  global : bool;
}
[@@deriving sexp]

type variable_table = (string, variable_entry) Hashtbl.Poly.t
[@@deriving sexp]

let sexp_of_variable_table (vtable : var