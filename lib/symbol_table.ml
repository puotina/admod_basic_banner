open Core_kernel
open Batsh_ast

type variable_entry = {
  name : string;
  global : bool;
}
[@@deriving sexp]

type variable_table = (string,