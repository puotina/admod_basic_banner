open Core_kernel

let print_indent (buf : Buffer.t) (indent : int) =
  Buffer.add_string buf (String.make indent ' ')

let print_statements
    (buf : Buffer.t)
    (stmts : 'a list)
    ~(f : Buffer.t -> 'a -> indent:int -> unit)
    ~(indent : int) =
  let print_statement_indented buf stmt = f buf s