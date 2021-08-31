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
    | FunctionScope of (string * variable_table)
  [@@deriving sexp]

  let is_function (scope : t) : bool =
    match scope with
    | GlobalScope _ -> false
    | FunctionScope _ -> true

  let variables (scope : t) : variable_table =
    match scope with
    | GlobalScope variables -> variables
    | FunctionScope (_, variables) -> variables

  let find_variable
      (scope: t)
      ~(name: string)
    : variable_entry option =
    Hashtbl.find (variables scope) name

  let is_global_variable
      (scope : t)
      ~(name : string)
    : bool =
    match scope with
    | GlobalScope _ -> true
    | FunctionScope (_, variable_table) ->
      match Hashtbl.find variable_table name with
      | None -> true (* if variable is not found, consider it as external *)
      | Some variable -> variable.global

  let fold
      (scope: t)
      ~(init: 'a)
      ~(f: string -> bool -> 'a -> 'a) =
    let vtable = variables scope in
    Hashtbl.fold vtable ~init
      ~f: (fun ~key:_ ~data acc -> f data.name data.global acc)

  let add_temporary_variable
      (scope: t)
    : identifier =
    let rec find_available_name (num : int) : string =
      let name = "_" ^ (Int.to_string num) in
      match find_variable scope ~name with
      | None ->
        (* Add to symbol table *)
        let variables = variables scope in
        Hashtbl.add_exn variables ~key: name ~data: {name; global = false};
        name
      | Some _ ->
        (* Duplicated, try again *)
        find_available_name (num + 1)
    in
    find_available_name 0
end

let process_identifier
    (scope: variable_table)
    (ident: identifier)
    ~(global: bool) =
  Hashtbl.change scope ident ~f:(fun original ->
      let entry = Some {
