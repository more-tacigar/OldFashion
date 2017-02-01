/* ============================================================
 * Copyright (c) 2017 tacigar. All rights reserved.
 * https://github.com/tacigar/oldfashion
 * ============================================================ */

%{
  open Ofsyntax
%}

%token<string> IDENTIFIER
%token<float> NUMERIC_LITERAL
%token<string> STRING_LITERAL
%token COMMA SEMI
%token FN VAR RETURN
%token IF ELSE FOR WHILE
%token PLUS MINUS MULT DIV
%token LE LT GE GT NE EQ OR AND
%token ASSIGN
%token TRUE FALSE
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token EOF

%left OR
%left AND
%left LT LE GT GE EQ NE
%left PLUS MINUS
%left MULT DIV
%nonassoc UMINUS UPLUS

%start program
%type<Ofast.program> program

%%

program
  : defs = list(external_definition); EOF
    { defs }
  ;
external_definition
  : FN; funcname = IDENTIFIER; LPAREN; params = separated_list(COMMA, IDENTIFIER); RPAREN; stmts = block
    {
      Ofast.Function_external_definition (funcname, params, stmts)
    }
  | VAR; varname = IDENTIFIER; ASSIGN; exp = expression
    {
      Ofast.Variable_external_definition (varname, Some exp)
    }
  | VAR; varname = IDENTIFIER
    {
      Ofast.Variable_external_definition (varname, None)
    }
  ;
block
  : LBRACE; stmts = list(statement); RBRACE
    {
      stmts
    }
  ;
statement
  : IF; LPAREN; cond = expression; RPAREN; tstmts = block
    {
      Ofast.If_statement (cond, tstmts, None)
    }
  | IF; LPAREN; cond = expression; RPAREN; tstmts = block; ELSE; fstmts = block
    {
      Ofast.If_statement (cond, tstmts, Some fstmts)
    }
  | FOR; LPAREN; init_stmt = statement; SEMI; cond = expression; SEMI; prop_stmt = statement; RPAREN; stmts = block
    {
      Ofast.For_statement (init_stmt, cond, prop_stmt, stmts)
    }
  | WHILE; LPAREN; cond = expression; RPAREN; stmts = block
    {
      Ofast.While_statement (cond, stmts)
    }
  | VAR; varname = IDENTIFIER; ASSIGN; exp = expression
    {
      Ofast.Variable_declaration_statement (varname, Some exp)
    }
  | VAR; varname = IDENTIFIER
    {
      Ofast.Variable_declaration_statement (varname, None)
    }
  | varname = IDENTIFIER; ASSIGN; exp = expression
    {
      Ofast.Variable_assign_statement (varname, exp)
    }
  | varname = IDENTIFIER; LBRACKET; key = expression; RBRACKET; ASSIGN; value = expression
    {
      Ofast.Table_value_assign_statement (varname, key, value)
    }
  | funcname = IDENTIFIER; LPAREN; args = separated_list(COMMA, expression); RPAREN
    {
      Ofast.Function_call_statement (funcname, args)
    }
  | RETURN
    {
      Ofast.Return_statement None
    }
  | RETURN; exp = expression
    {
      Ofast.Return_statement (Some exp)
    }
  ;
expression
  : varname = IDENTIFIER
    {
      Ofast.Variable_expression (varname)
    }
  | num = NUMERIC_LITERAL
    {
      Ofast.Numeric_literal_expression (num)
    }
  | str = STRING_LITERAL
    {
      Ofast.String_literal_expression (str)
    }
  | TRUE
    {
      Ofast.Boolean_literal_expression (true)
    }
  | FALSE
    {
      Ofast.Boolean_literal_expression (false)
    }
  | LBRACKET; fields = separated_list(COMMA, field); RBRACKET
    {
      Ofast.Table_constructor_expression (fields)
    }
  | varname = IDENTIFIER; LBRACKET; exp = expression; RBRACKET
    {
      Ofast.Table_value_expression (varname, exp)
    }
  | PLUS; exp = expression %prec UPLUS
    {
      Ofast.Unary_operation_expression (Ofast.Uplus, exp)
    }
  | MINUS; exp = expression %prec UMINUS
    {
      Ofast.Unary_operation_expression (Ofast.Uminus, exp)
    }
  | lhs = expression; PLUS; rhs = expression
    {
      Ofast.Binary_operation_expression (lhs, Ofast.Plus, rhs)
    }
  | lhs = expression; MINUS; rhs = expression
    {
      Ofast.Binary_operation_expression (lhs, Ofast.Minus, rhs)
    }
  | lhs = expression; MULT; rhs = expression
    {
      Ofast.Binary_operation_expression (lhs, Ofast.Mult, rhs)
    }
  | lhs = expression; DIV; rhs = expression
    {
      Ofast.Binary_operation_expression (lhs, Ofast.Div, rhs)
    }
  | lhs = expression; LT; rhs = expression
    {
      Ofast.Binary_operation_expression (lhs, Ofast.Lt, rhs)
    }
  | lhs = expression; LE; rhs = expression
    {
      Ofast.Binary_operation_expression (lhs, Ofast.Le, rhs)
    }
  | lhs = expression; GT; rhs = expression
    {
      Ofast.Binary_operation_expression (lhs, Ofast.Gt, rhs)
    }
  | lhs = expression; GE; rhs = expression
    {
      Ofast.Binary_operation_expression (lhs, Ofast.Ge, rhs)
    }
  | lhs = expression; NE; rhs = expression
    {
      Ofast.Binary_operation_expression (lhs, Ofast.Ne, rhs)
    }
  | lhs = expression; EQ; rhs = expression
    {
      Ofast.Binary_operation_expression (lhs, Ofast.Eq, rhs)
    }
  | lhs = expression; AND; rhs = expression
    {
      Ofast.Binary_operation_expression (lhs, Ofast.And, rhs)
    }
  | lhs = expression; OR; rhs = expression
    {
      Ofast.Binary_operation_expression (lhs, Ofast.Or, rhs)
    }
  | funcname = IDENTIFIER; LPAREN; args = separated_list(COMMA, expression); RPAREN
    {
      Ofast.Function_call_expression (funcname, args)
    }
  | LPAREN; exp = expression; RPAREN
    {
      Ofast.Paren_expression (exp)
    }
  ;
field
  : key = expression; ASSIGN; value = expression
    {
      (key, value)
    }
  ;
