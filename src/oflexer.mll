(* ============================================================
 * Copyright (c) 2017 tacigar. All rights reserved.
 * https://github.com/tacigar/oldfashion
 * ============================================================ *)

{
  exception Syntax_error of string

  open Ofparser
  open Lexing

  let reserved_words = [
    ("fn", FN);
    ("var", VAR);
    ("return", RETURN);
    ("if", IF);
    ("else", ELSE);
    ("for", FOR);
    ("while", WHILE);
    ("true", TRUE);
    ("false", FALSE);
    ("and", AND);
    ("or", OR);
  ]

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1;
      }
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let int = ['1'-'9'] ['0'-'9']*
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?
let number = int | float

rule read = parse 
  | white
    { read lexbuf }
  | newline
    { next_line lexbuf; read lexbuf }
  | int
    { let num = int_of_string (lexeme lexbuf) in
      NUMERIC_LITERAL (float_of_int num)
    }
  | float
    { NUMERIC_LITERAL (float_of_string (lexeme lexbuf)) }
  | '"'
    { read_string (Buffer.create 17) lexbuf }
  | ','
    { COMMA }
  | ';'
    { SEMI }
  | '+'
    { PLUS }
  | '-'
    { MINUS }
  | '*'
    { MULT }
  | '/'
    { DIV }
  | '<'
    { LT }
  | "<="
    { LE }
  | '>'
    { GT }
  | ">="
    { GE }
  | "!="
    { NE }
  | "=="
    { EQ }
  | '='
    { ASSIGN }
  | '('
    { LPAREN }
  | ')'
    { RPAREN }
  | '{'
    { LBRACE }
  | '}'
    { RBRACE }
  | '['
    { LBRACKET }
  | ']'
    { RBRACKET }
  | eof
    { EOF }
  | id
    { let id = lexeme lexbuf in
      try
        List.assoc id reserved_words
      with
      _ -> IDENTIFIER id
    }
  
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
  