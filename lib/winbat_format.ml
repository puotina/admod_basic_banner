
open Core_kernel
open Winbat_ast

let escape (str : string) : string =
  let buffer = Buffer.create (String.length str) in
  let exclamation = match String.index str '!' with
    | None -> false
    | Some _ -> true