type t

exception ParseError of string
exception SemanticError of string

val create_from_file : string -> t
val create_from_channel : in_channel -> string -> t
val create_from_string : string -> t
val prettify : t -