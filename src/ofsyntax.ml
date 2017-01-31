(* ============================================================
 * Copyright (c) 2017 tacigar. All rights reserved.
 * https://github.com/tacigar/oldfashion
 * ============================================================ *)

module Ast = struct
  type identifier = string
   and program = external_definition list
   and external_definition =
     | Function_external_definition of identifier * (identifier list) * (statement list)
     | Variable_external_definition of identifier * (expression option)
   and statement =
     | If_statement of expression * (statement list) * ((statement list) option)
     | For_statement of statement * expression * statement * (statement list)
     | While_statement of expression * (statement list)
     | Variable_declaration_statement of identifier * (expression option)
     | Variable_assign_statement of identifier * expression
     | Table_value_assign_statement of identifier * expression * expression
     | Function_call_statement of identifier * (expression list)
     | Return_statement of expression option
   and expression =
     | Variable_expression of identifier
     | Boolean_literal_expression of bool
     | Numeric_literal_expression of float
     | String_literal_expression of string
     | Table_constructor_expression of (expression * expression) list
     | Table_value_expression of identifier * expression
     | Unary_operation_expression of unary_operator * expression
     | Binary_operation_expression of expression * binary_operator * expression
     | Function_call_expression of identifier * (expression list)
     | Paren_expression of expression
   and unary_operator =
     | Uplus | Uminus
   and binary_operator =
     | Plus | Minus | Mult | Div
     | Lt | Le | Gt | Ge | Ne | Eq
     | And | Or

  let to_string program =
    let rec write_line buf depth str =
      for i = 0 to depth do
        Buffer.add_string buf "  "
      done;
      Buffer.add_string buf str;
      Buffer.add_char buf '\n'

    and translate_external_definition buf depth = function
      | Function_external_definition (funcname, params, stmts) ->
         translate_function_external_definition buf depth (funcname, params, stmts)
      | Variable_external_definition (varname, exp_option) ->
         translate_variable_external_definition buf depth (varname, exp_option)

    and translate_function_external_definition buf depth (funcname, params, stmts) =
      write_line buf depth "function external definition:";
      write_line buf (depth + 1) "function name:";
      write_line buf (depth + 2) funcname;
      write_line buf (depth + 1) "parameters:";
      List.iter (fun param ->
          write_line buf (depth + 2) param
        ) params;
      write_line buf (depth + 1) "function body:";
      List.iter (fun stmt ->
          translate_statement buf (depth + 2) stmt
        ) stmts

    and translate_variable_external_definition buf depth (varname, x) =
      write_line buf depth "variable external definition:";
      write_line buf (depth + 1) "variable name:";
      write_line buf (depth + 2) varname;
      match x with
      | Some exp ->
         write_line buf (depth + 1) "initializer expression:";
         translate_expression buf (depth + 2) exp
      | _ -> ()

    and translate_statement buf depth = function
      | If_statement (cond, tstmt, fstmt_option) ->
         translate_if_statement buf depth (cond, tstmt, fstmt_option)
      | For_statement (init_stmt, cond, prop_stmt, stmts) ->
         translate_for_statement buf depth (init_stmt, cond, prop_stmt, stmts)
      | While_statement (cond, stmts) ->
         translate_while_statement buf depth (cond, stmts)
      | Variable_declaration_statement (varname, exp_option) ->
         translate_variable_declaration_statement buf depth (varname, exp_option)
      | Variable_assign_statement (varname, exp) ->
         translate_assign_statement buf depth (varname, exp)
      | Table_value_assign_statement (varname, key, value) ->
         translate_table_value_assign_statement buf depth (varname, key, value)
      | Function_call_statement (funcname, args) ->
         translate_function_call_statement buf depth (funcname, args)
      | Return_statement exp_option ->
         translate_return_statement buf depth exp_option

    and translate_if_statement buf depth (cond, tstmt, fstmt_option) =
      write_line buf depth "if statement:";
      write_line buf (depth + 1) "condition:";
      translate_expression buf (depth + 2) cond;
      write_line buf (depth + 1) "true statements:";
      List.iter (fun stmt ->
          translate_statement buf (depth + 2) stmt
        ) tstmt;
      match fstmt_option with
      | Some fstmt ->
         write_line buf (depth + 1) "false statements:";
         List.iter (fun stmt ->
             translate_statement buf (depth + 2) stmt
           ) fstmt
      | _ -> ()

    and translate_for_statement buf depth (init_stmt, cond, prop_stmt, stmts) =
      write_line buf depth "for statement:";
      write_line buf (depth + 1) "init statement:";
      translate_statement buf (depth + 2) init_stmt;
      write_line buf (depth + 1) "condition:";
      translate_expression buf (depth + 2) cond;
      write_line buf (depth + 1) "prop statement:";
      translate_statement buf (depth + 2) prop_stmt;
      write_line buf (depth + 1) "statements:";
      List.iter (fun stmt ->
          translate_statement buf (depth + 2) stmt
        ) stmts

    and translate_while_statement buf depth (cond, stmts) =
      write_line buf depth "while statement:";
      write_line buf (depth + 1) "condition:";
      translate_expression buf (depth + 2) cond;
      write_line buf (depth + 1) "statements:";
      List.iter (fun stmt ->
          translate_statement buf (depth + 2) stmt
        ) stmts

    and translate_variable_declaration_statement buf depth (varname, x) =
      write_line buf depth "variable declaration statement:";
      write_line buf (depth + 1) "variable name:";
      write_line buf (depth + 2) varname;
      match x with
      | Some exp ->
         write_line buf (depth + 1) "initializer expression:";
         translate_expression buf (depth + 2) exp
      | _ -> ()

    and translate_assign_statement buf depth (varname, exp) =
      write_line buf depth "variable assign statement:";
      write_line buf (depth + 1) "variable name:";
      write_line buf (depth + 2) varname;
      write_line buf (depth + 1) "expression:";
      translate_expression buf (depth + 2) exp

    and translate_table_value_assign_statement buf depth (varname, key, value) =
      write_line buf depth "table value assign statement:";
      write_line buf (depth + 1) "variable name:";
      write_line buf (depth + 2) varname;
      write_line buf (depth + 1) "key expression:";
      translate_expression buf (depth + 2) key;
      write_line buf (depth + 1) "value expression:";
      translate_expression buf (depth + 2) value

    and translate_function_call_statement buf depth (funcname, args) =
      write_line buf depth "function call statement:";
      write_line buf (depth + 1) "function name:";
      write_line buf (depth + 2) funcname;
      write_line buf (depth + 1) "arguments:";
      List.iter (fun arg ->
          translate_expression buf (depth + 2) arg
        ) args

    and translate_return_statement buf depth x =
      write_line buf depth "return statement:";
      match x with
      | Some exp ->
         write_line buf (depth + 1) "return value:";
         translate_expression buf (depth + 2) exp
      | _ -> ()

    and translate_expression buf depth = function
      | Variable_expression varname ->
         translate_variable_expression buf depth varname
      | Boolean_literal_expression b ->
         translate_boolean_literal_expression buf depth b
      | Numeric_literal_expression n ->
         translate_numeric_literal_expression buf depth n
      | String_literal_expression s ->
         translate_string_literal_expression buf depth s
      | Table_constructor_expression fields ->
         translate_table_constructor_expression buf depth fields
      | Table_value_expression (varname, key) ->
         translate_table_value_expression buf depth (varname, key)
      | Unary_operation_expression (uop, exp) ->
         translate_unary_operation_expression buf depth (uop, exp)
      | Binary_operation_expression (lhs, op, rhs) ->
         translate_binary_operation_expression buf depth (lhs, op, rhs)
      | Function_call_expression (funcname, args) ->
         translate_function_call_expression buf depth (funcname, args)
      | Paren_expression (exp) ->
         translate_paren_expression buf depth exp

    and translate_variable_expression buf depth varname =
      write_line buf depth "variable expression:";
      write_line buf (depth + 1) "variable name:";
      write_line buf (depth + 2) varname

    and translate_boolean_literal_expression buf depth b =
      write_line buf depth "boolean literal expression:";
      write_line buf (depth + 1) "value:";
      write_line buf (depth + 2) (string_of_bool b)

    and translate_numeric_literal_expression buf depth n =
      write_line buf depth "numeric literal expression:";
      write_line buf (depth + 1) "value:";
      write_line buf (depth + 2) (string_of_float n)

    and translate_string_literal_expression buf depth s =
      write_line buf depth "string literal expression:";
      write_line buf (depth + 1) "value:";
      write_line buf (depth + 2) s

    and translate_table_constructor_expression buf depth fields =
      write_line buf depth "table constructor expression:";
      write_line buf (depth + 1) "fields:";
      List.iter (fun field ->
          write_line buf (depth + 2) "key:";
          translate_expression buf (depth + 3) (fst field);
          write_line buf (depth + 2) "value:";
          translate_expression buf (depth + 3) (snd field)
        ) fields

    and translate_table_value_expression buf depth (varname, key) =
      write_line buf depth "table value expression:";
      write_line buf (depth + 1) "variable name:";
      write_line buf (depth + 2) varname;
      write_line buf (depth + 1) "key expression:";
      translate_expression buf (depth + 2) key

    and translate_unary_operation_expression buf depth (uop, exp) =
      write_line buf depth "uanry operation expression:";
      write_line buf (depth + 1) "operator:";
      translate_unary_operator buf (depth + 2) uop;
      write_line buf (depth + 1) "expression:";
      translate_expression buf (depth + 2) exp

    and translate_binary_operation_expression buf depth (lhs, bop, rhs) =
      write_line buf depth "binary operation expression:";
      write_line buf (depth + 1) "left hand side expression:";
      translate_expression buf (depth + 2) lhs;
      write_line buf (depth + 1) "operator:";
      translate_binary_operator buf (depth + 2) bop;
      write_line buf (depth + 1) "right hand side expression:";
      translate_expression buf (depth + 2) rhs

    and translate_function_call_expression buf depth (funcname, args) =
      write_line buf depth "function call expression:";
      write_line buf (depth + 1) "function name:";
      write_line buf (depth + 2) funcname;
      write_line buf (depth + 1) "arguments";
      List.iter (fun arg ->
          translate_expression buf (depth + 2) arg
        ) args

    and translate_paren_expression buf depth exp =
      write_line buf depth "paren expression:";
      write_line buf (depth + 1) "expression:";
      translate_expression buf (depth + 2) exp

    and translate_unary_operator buf depth = function
      | Uplus ->
         write_line buf depth "+"
      | Uminus ->
         write_line buf depth "-"

    and translate_binary_operator buf depth = function
      | Plus ->
         write_line buf depth "+"
      | Minus ->
         write_line buf depth "-"
      | Mult ->
         write_line buf depth "*"
      | Div ->
         write_line buf depth "/"
      | Lt ->
         write_line buf depth "<"
      | Le ->
         write_line buf depth "<="
      | Gt ->
         write_line buf depth ">"
      | Ge ->
         write_line buf depth ">="
      | Ne ->
         write_line buf depth "!="
      | Eq ->
         write_line buf depth "=="
      | And ->
         write_line buf depth "and"
      | Or ->
         write_line buf depth "or"
    in

    let buf = Buffer.create 100 in
    List.iter (fun external_definition ->
        translate_external_definition buf 0 external_definition
      ) program;
    Buffer.contents buf
end
