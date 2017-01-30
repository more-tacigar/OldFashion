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
%type<Ofsyntax.Ast.program> program

%%

program
  : defs = list(external_definition); EOF
    { defs }
  ;
external_definition
  : FN; funcname = IDENTIFIER; LPAREN; params = separated_list(COMMA, IDENTIFIER); RPAREN; stmts = block
    {
      Ofsyntax.Ast.Function_external_definition (funcname, params, stmts)
    }
  | VAR; varname = IDENTIFIER; ASSIGN; exp = expression
    {
      Ofsyntax.Ast.Variable_external_definition (varname, Some exp)
    }
  | VAR; varname = IDENTIFIER
    {
      Ofsyntax.Ast.Variable_external_definition (varname, None)
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
      Ofsyntax.Ast.If_statement (cond, tstmts, None) 
    }
  | IF; LPAREN; cond = expression; RPAREN; tstmts = block; ELSE; fstmts = block
    {
      Ofsyntax.Ast.If_statement (cond, tstmts, Some fstmts)
    }
  | FOR; LPAREN; init_stmt = statement; SEMI; cond = expression; SEMI; prop_stmt = statement; RPAREN; stmts = block
    {
      Ofsyntax.Ast.For_statement (init_stmt, cond, prop_stmt, stmts)
    }
  | WHILE; LPAREN; cond = expression; RPAREN; stmts = block
    {
      Ofsyntax.Ast.While_statement (cond, stmts)
    }
  | VAR; varname = IDENTIFIER; ASSIGN; exp = expression
    {
      Ofsyntax.Ast.Variable_declaration_statement (varname, Some exp)
    }
  | VAR; varname = IDENTIFIER
    {
      Ofsyntax.Ast.Variable_declaration_statement (varname, None)
    }
  | varname = IDENTIFIER; ASSIGN; exp = expression
    {
      Ofsyntax.Ast.Variable_assign_statement (varname, exp)
    }
  | varname = IDENTIFIER; LBRACKET; key = expression; RBRACKET; ASSIGN; value = expression
    {
      Ofsyntax.Ast.Table_value_assign_statement (varname, key, value)
    }
  | funcname = IDENTIFIER; LPAREN; args = separated_list(COMMA, expression); RPAREN
    {
      Ofsyntax.Ast.Function_call_statement (funcname, args)
    }
  | RETURN
    {
      Ofsyntax.Ast.Return_statement None
    }
  | RETURN; exp = expression
    {
      Ofsyntax.Ast.Return_statement (Some exp)
    }
  ;
expression
  : varname = IDENTIFIER
    {
      Ofsyntax.Ast.Variable_expression (varname)
    }
  | num = NUMERIC_LITERAL
    {
      Ofsyntax.Ast.Numeric_literal_expression (num)
    }
  | str = STRING_LITERAL
    {
      Ofsyntax.Ast.String_literal_expression (str)
    }
  | TRUE
    {
      Ofsyntax.Ast.Boolean_literal_expression (true)
    }
  | FALSE
    {
      Ofsyntax.Ast.Boolean_literal_expression (false)
    }
  | LBRACKET; fields = separated_list(COMMA, field); RBRACKET
    {
      Ofsyntax.Ast.Table_constructor_expression (fields)
    }
  | varname = IDENTIFIER; LBRACKET; exp = expression; RBRACKET
    {
      Ofsyntax.Ast.Table_value_expression (varname, exp)
    }
  | PLUS; exp = expression %prec UPLUS
    {
      Ofsyntax.Ast.Unary_operation_expression (Ofsyntax.Ast.Uplus, exp)
    }
  | MINUS; exp = expression %prec UMINUS
    {
      Ofsyntax.Ast.Unary_operation_expression (Ofsyntax.Ast.Uminus, exp)
    }
  | lhs = expression; PLUS; rhs = expression
    {
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Plus, rhs)
    }
  | lhs = expression; MINUS; rhs = expression
    {
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Minus, rhs)
    }
  | lhs = expression; MULT; rhs = expression
    {
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Mult, rhs)
    }
  | lhs = expression; DIV; rhs = expression
    {
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Div, rhs)
    }
  | lhs = expression; LT; rhs = expression
    {
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Lt, rhs)
    }
  | lhs = expression; LE; rhs = expression
    {
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Le, rhs)
    }
  | lhs = expression; GT; rhs = expression
    {
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Gt, rhs)
    }
  | lhs = expression; GE; rhs = expression
    {
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Ge, rhs)
    }
  | lhs = expression; NE; rhs = expression
    {
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Ne, rhs)
    }
  | lhs = expression; EQ; rhs = expression
    {
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Eq, rhs)
    }
  | lhs = expression; AND; rhs = expression
    {
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.And, rhs)
    }
  | lhs = expression; OR; rhs = expression
    {
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Or, rhs)
    }
  | funcname = IDENTIFIER; LPAREN; args = separated_list(COMMA, expression); RPAREN
    {
      Ofsyntax.Ast.Function_call_expression (funcname, args)
    }
  ;
field
  : key = expression; ASSIGN; value = expression
    {
      (key, value)
    }
  ;
