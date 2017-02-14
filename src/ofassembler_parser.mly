/* ============================================================
 * Copyright (c) 2017 tacigar. All rights reserved.
 * https://github.com/tacigar/oldfashion
 * ============================================================ */

%token<string> IDENTIFIER
%token<int> INTEGER_LITERAL
%token<float> NUMERIC_LITERAL
%token<string> STRING_LITERAL
%token DEF ARGS LOCALS LABEL
%token COLON ASSIGN COMMA
%token NEWLINE EOF

%start program
%type<Ofassembler_ast.program> program

%%

program
  : codes = list(code); EOF
    { codes }
  ;
code
  : DEF; funcname = IDENTIFIER; COLON; ARGS; ASSIGN; args = INTEGER_LITERAL; COMMA; LOCALS; ASSIGN; locals = INTEGER_LITERAL; NEWLINE
    { Ofassembler_ast.Function_define (funcname, args, locals) }
  | LABEL; n = INTEGER_LITERAL; NEWLINE
    { Ofassembler_ast.Label_define n }
  | instruction = IDENTIFIER; arg = option(argument); NEWLINE
    { Ofassembler_ast.Instruction (instruction, arg) }
  ;
argument
  : s = STRING_LITERAL
    { Ofassembler_ast.String_literal s }
  | n = INTEGER_LITERAL
    { Ofassembler_ast.Integer_literal n }
  | n = NUMERIC_LITERAL
    { Ofassembler_ast.Numeric_literal n }
  | funcname = IDENTIFIER
    { Ofassembler_ast.Function_name funcname }
  ;
  