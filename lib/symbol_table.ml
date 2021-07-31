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
                   let item = (sexp_of_variable_entry data) in
                   item :: acc
                 )
            )

type function_entry =
  | Declaration
  | Defination of variable_table
[@@deriving sexp]

type t = {
  functions : (string, function_entry) Hashtbl.Poly.t;
  globals : variable_table;
}
[@@deriving sexp]

module Scope = struct
  type t =
    | GlobalScope of variable_table
    | Functio