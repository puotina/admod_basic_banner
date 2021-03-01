open Core_kernel

type identifier = string

and identifiers = identifier list

and leftvalue =
  | Identifier of identifier
  | ListAccess of (leftvalue * arithmetic)
  | EntireList of leftvalue
  | Cardinal of leftvalue

and arithmetic =
  | Leftvalue of leftvalue
  | Int of int
  | Float of float
  | ArithUnary of (string * arithmetic)
  | ArithBinary of (string * arithmetic * arithmetic)

and expression =
  | Vari