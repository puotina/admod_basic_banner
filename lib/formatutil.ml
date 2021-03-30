open Core_kernel

let print_indent (buf : Buffer.t) (indent : int) =
  Buffer.add_string buf (String.make indent