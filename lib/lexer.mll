
{
open Lexing
open Parser_yacc

exception SyntaxError of string

let next_line (lexbuf: Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;