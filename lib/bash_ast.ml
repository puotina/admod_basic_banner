open Core_kernel

type identifier = string

and identifiers = identifier list

and leftvalue =
  | Identifier of identifier
  | Lis