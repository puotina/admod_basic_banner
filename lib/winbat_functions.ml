open Core_kernel
open Winbat_ast

let rec expand_command (name : varstrings) (args : parameters) =
  ma