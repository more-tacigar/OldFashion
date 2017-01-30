(* ============================================================
 * Copyright (c) 2017 tacigar. All rights reserved.
 * https://github.com/tacigar/oldfashion
 * ============================================================ *)

module Ast = struct
  type program = external_definition list
   and external_definition =
     | Function_external_definition of identifier * (identifier list) * (statement list)
     | Variable_external_definition of identifier * (expression option)
   and statement =
     | If_statement of expression * (statement list) * ((statement list) option)
     | For_statement of statement * expression * statement * (statement list)
     | While_statement of expression * (statement list)
     | Variable_declaration_statement of identifier * (expression option)
     | Variable_assign_statement of identifier * expression
     | Function_call_statement of identifier * (expression list)
     | Return_statement of expression option
   and expression =
     | Variable_expression of identifier
     | Boolean_literal_expression of bool
     | Numeric_literal_expression of float
     | String_literal_expression of string
     | Binary_operation_expression of expression * binary_operator * exptression
     | Function_call_expression of identifier * (expression list)
   and binary_operator =
     | Plus | Minus | Mult | Div
     | Lt | Le | Gt | Ge | Ne | Eq
     | And | Or
end
