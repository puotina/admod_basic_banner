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
    `Call ([`Str "echo | set /p ="], [] :: args)
  | [`Str "call"] -> (
      match args with
      | cmd :: real_args ->
        expand_command cmd real_args
      | [] ->
        failwith "call must have at least 1 argument."
    )
  | [`Str "readdir"] ->
    `Call ([`Str "dir /w"], args)
  | _ ->
    `Call (name, args)

let rec expand_statement (stmt : statement) : statement =
  match stmt with
  | 