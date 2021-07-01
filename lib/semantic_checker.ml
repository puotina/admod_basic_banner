open Core_kernel
open Batsh_ast

exception Error of string

let check_function