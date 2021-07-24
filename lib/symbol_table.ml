open Core_kernel
open Batsh_ast

type variable_entry = {
  name : string;
  global : bool;
}
[@@deriving sexp]

type variable_table = (string, variable_entry) Hashtbl.Poly.t
[@@deriving sexp]

let sexp_of_variable_table (vtable : variable_table) : Sexp.t =
  Sexp.List (Hashtbl.fold vtable ~init: []
               ~f: (fun ~key:_ ~data acc ->
                   let item = (sexp_of_variable_entry 