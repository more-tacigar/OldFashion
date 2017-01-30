(* ============================================================
 * Copyright (c) 2017 tacigar. All rights reserved.
 * https://github.com/tacigar/oldfashion
 * ============================================================ *)
 
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
%type<Ast.program> program

%%

program
  : defs = list(external_definition); EOF
    { defs }
  ;
external_definition
  : FN; funcname = IDENTIFIER; LPAREN; params = separated_list(COMMA, IDENTIFIER); RPAREN; stmts = block
    {
      Ast.Function_external_definition (funcname, params, stmts)
    }
  | VAR; varname = IDENTIFIER; ASSIGN; exp = expression
    {
      Ast.Variable_external_definition (varname, Some exp)
    }
  | VAR; varname = IDENTIFIER
    {
      Ast.Variable_external_definition (varname, None)
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
      Ast.If_statement (cond, tstmts, None) 
    }
  | IF; LPAREN; cond = expression; RPAREN; tstmts = block; ELSE; fstmts = block
    {
      Ast.If_statement (cond, tstmts, Some fstmts)
    }
  | FOR; LPAREN; init_stmt = statement; SEMI; cond = expression; SEMI; prop_stmt = statement; RPAREN; stmts = block
    {
      Ast.For_statement (init_stmt, cond, prop_stmt, stmts)
    }
  | WHILE; LPAREN; cond = expression; RPAREN; stmts = block
    {
      Ast.While_statement (cond, stmts)
    }
  | VAR; varname = IDENTIFIER; ASSIGN; exp = expression
    {
      Ast.Variable_declaration_statement (varname, Some exp)
    }
  | VAR; varname = IDENTIFIER
    {
      Ast.Variable_declaration_statement (varname, None)
    }
  | varname = IDENTIFIER; ASSIGN; exp = expression
    {
      Ast.Variable_assign_statement (varname, exp)
    }
  | varname = IDENTIFIER; LBRACKET; key = expression; RBRACKET; ASSIGN; value = expression
    {
      Ast.Table_value_assign_statement (varname, key, value)
    }
  | funcname = IDENTIFIER; LPAREN; args = separated_list(COMMA, expression); RPAREN
    {
      Ast.Function_call_statement (funcname, args)
    }
  | RETURN
    {
      Ast.Return_statement None
    }
  | RETURN; exp = expression
    {
      Ast.Return_statement (Some exp)
    }
  ;
expression
  : varname = IDENTIFIER
    {
      Ast.Variable_expression (varname)
    }
  | num = NUMERIC_LITERAL
    {
      Ast.Numeric_literal_expression (num)
    }
  | str = STRING_LITERAL
    {
      Ast.String_literal_expression (str)
    }
  | TRUE
    {
      Ast.Boolean_literal_expression (true)
    }
  | FALSE
    {
      Ast.Boolean_literal_expression (false)
    }
  | LBRACKET; fields = separated_list(COMMA, field); RBRACKET
    {
      Ast.Table_constructor_expression (fields)
    }
  | varname = IDENTIFIER; LBRACKET; exp = expression; RBRACKET
    {
      Ast.Table_value_expression (varname, exp)
    }
  | PLUS; exp = expression %pred UPLUS
    {
      Ast.Unary_operation_expression (Ast.Uplus, exp)
    }
  | MINUS; exp = expression %prec UMINUS
    {
      Ast.Unary_operation_expression (Ast.Uminus, exp)
    }
  | lhs = expression; PLUS; rhs = expression
    {
      Ast.Binary_operation_expression (lhs, Ast.Plus, rhs)
    }
  | lhs = expression; MINUS; rhs = expression
    {
      Ast.Binary_operation_expression (lhs, Ast.Minus, rhs)
    }
  | lhs = expression; MULT; rhs = expression
    {
      Ast.Binary_operation_expression (lhs, Ast.Mult, rhs)
    }
  | lhs = expression; DIV; rhs = expression
    {
      Ast.Binary_operation_expression (lhs, Ast.Div, rhs)
    }
  | lhs = expression; LT; rhs = expression
    {
      Ast.Binary_operation_expression (lhs, Ast.Lt, rhs)
    }
  | lhs = expression; LE; rhs = expression
    {
      Ast.Binary_operation_expression (lhs, Ast.Le, rhs)
    }
  | lhs = expression; GT; rhs = expression
    {
      Ast.Binary_operation_expression (lhs, Ast.Gt, rhs)
    }
  | lhs = expression; GE; rhs = expression
    {
      Ast.Binary_operation_expression (lhs, Ast.Ge, rhs)
    }
  | lhs = expression; NE; rhs = expression
    {
      Ast.Binary_operation_expression (lhs, Ast.Ne, rhs)
    }
  | lhs = expression; EQ; rhs = expression
    {
      Ast.Binary_operation_expression (lhs, Ast.Eq, rhs)
    }
  | lhs = expression; AND; rhs = expression
    {
      Ast.Binary_operation_expression (lhs, Ast.And, rhs)
    }
  | lhs = expression; OR; rhs = expression
    {
      Ast.Binary_operation_expression (lhs, Ast.Or, rhs)
    }
  | funcname = IDENTIFIER; LPAREN; args = separated_list(COMMA, expression); RPAREN
    {
      Ast.Function_call_expression (funcname, args)
    }
  ;
field
  : key = expression; ASSIGN; value = expression
    {
      (key, value)
    }
  ;
