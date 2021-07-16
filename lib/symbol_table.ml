open Core_kernel
open Batsh_ast

type variable_entry = {
  name : string;
  global : boo