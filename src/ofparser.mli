
(* The type of tokens. *)

type token = 
  | WHILE
  | VAR
  | TRUE
  | STRING_LITERAL of (string)
  | SEMI
  | RPAREN
  | RETURN
  | RBRACKET
  | RBRACE
  | PLUS
  | OR
  | NUMERIC_LITERAL of (float)
  | NE
  | MULT
  | MINUS
  | LT
  | LPAREN
  | LE
  | LBRACKET
  | LBRACE
  | IF
  | IDENTIFIER of (string)
  | GT
  | GE
  | FOR
  | FN
  | FALSE
  | EQ
  | EOF
  | ELSE
  | DIV
  | COMMA
  | ASSIGN
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ofsyntax.Ast.program)
