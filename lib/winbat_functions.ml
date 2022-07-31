open Core_kernel
open Winbat_ast

let rec expand_command (name : varstrings) (args : parameters) =
  match name with
  | [`Str "bash"] ->
    `Empty
  | [`Str "batch"] -> (
      match args with
   