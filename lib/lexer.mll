
{
open Lexing
open Parser_yacc

exception SyntaxError of string

let next_line (lexbuf: Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

}

let int = '-'? ['0'-'9'] ['0'-'9']*

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | "if"     { IF }
  | "else"   { ELSE }
  | "while"  { WHILE }
  | "function" { FUNCTION }
  | "global" { GLOBAL }
  | "return" { RETURN }
  | '='      { EQUAL }
  | '"'      { read_string (Buffer.create 17) lexbuf }
  | '('      { LEFT_PAREN }
  | ')'      { RIGHT_PAREN }
  | '{'      { LEFT_BRACE }
  | '}'      { RIGHT_BRACE }
  | '['      { LEFT_BRACK }
  | ']'      { RIGHT_BRACK }
  | ';'      { SEMICOLON }
  | ','      { COMMA }
  | '+'      { PLUS }
  | '-'      { MINUS }
  | '*'      { MULTIPLY }
  | '/'      { DIVIDE }
  | '%'      { MODULO }
  | "++"     { CONCAT }
  | '!'      { NOT }
  | "=="     { SEQ }
  | "!="     { SNE }
  | "==="    { AEQ }