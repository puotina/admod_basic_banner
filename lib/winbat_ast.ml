open Core_kernel

type identifier = string

and identifiers = identifier list

and label = string

and varint = [
  | `Var of leftvalue
  | `Int of int
]

and leftvalue = [
  | `Ide