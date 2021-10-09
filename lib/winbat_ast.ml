open Core_kernel

type identifier = string

and identifiers = identifier list

and label = string

and varint = [
  | `Var of leftvalue
  | `Int of int
]

and leftvalue = [
  | `Identifier of identifier
  | `ListAccess of (leftvalue * varint)
]

and arithmetic = [
  | `Var of leftvalue
  | `Int of int
  | `ArithUnary of (string * arithmetic)
  | `ArithBinary of (string * arithmetic * arit