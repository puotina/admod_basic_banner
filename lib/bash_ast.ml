open Core_kernel

type identifier = string

and identifiers = identifier list

and leftvalue =
  | Identifier of identifier
  | ListAccess of (leftvalue * arithmetic)
  | EntireList of leftvalue
  | Cardinal of leftval