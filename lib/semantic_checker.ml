open Core_kernel
open Batsh_ast

exception Error of string

let check_function_statement (stmt : statement) =
  match stmt with
  | Return (Some List _) ->
    raise (Error "list can not be used as a return value")
  | _ ->
    ()

le