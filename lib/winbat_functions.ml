open Core_kernel
open Winbat_ast

let rec expand_command (name : varstrings) (args : parameters) =
  match name with
  | [`Str "bash"] ->
    `Empty
  | [`Str "batch"] -> (
      match args with
      | [[`Str raw]] ->
        `Raw raw
      | _ ->
        failwith "batch raw command must have 1 argument of string literal."
    )
  | [`Str "println"] -> (
      match args with
      | [] ->
        `Call ([`Str "echo:"], [])
      | _ ->
        `Call ([`Str "echo"], args)
    )
  | [`Str "print"] ->
    `Ca