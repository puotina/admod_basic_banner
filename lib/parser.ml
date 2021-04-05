
open Core_kernel

type t = {
  lex: Lexing.lexbuf;
  ast: Batsh_ast.t;
  symtable: Symbol_table.t;
}

exception ParseError of string
exception SemanticError of string

let parse (lexbuf : Lexing.lexbuf) : Batsh_ast.t =
  let print_position () () =
    let pos = lexbuf.Lexing.lex_curr_p in
    sprintf "%s:%d:%d"
      pos.Lexing.pos_fname
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)