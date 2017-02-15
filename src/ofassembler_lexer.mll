(* ============================================================
 * Copyright (c) 2017 tacigar. All rights reserved.
 * https://github.com/tacigar/oldfashion
 * ============================================================ *)

{
  exception Syntax_error of string

  open Ofassembler_parser
  open Lexing
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let int = '0' | (['1'-'9'] ['0'-'9']*)
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

rule read = parse
  | white
    { read lexbuf }
  | newline
    { NEWLINE }
  | int
    { let n = int_of_string (lexeme lexbuf) in
      INTEGER_LITERAL n
    }
  | float
    { NUMERIC_LITERAL (float_of_string (lexeme lexbuf)) }
  | '"'
    { read_string (Buffer.create 17) lexbuf }
  | ','
    { COMMA }
  | ':'
    { COLON }
  | ".def"
    { DEF }
  | "args"
    { ARGS }
  | "locals"
    { LOCALS }
  | ".label"
    { LABEL }
  | '='
    { ASSIGN }
  | id 
    { IDENTIFIER (lexeme lexbuf) }
  | eof
    { EOF }

and read_string buf = parse
  | '"'       { STRING_LITERAL (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | eof { raise (Syntax_error ("String is not terminated")) }
  | _ { raise (Syntax_error ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  
