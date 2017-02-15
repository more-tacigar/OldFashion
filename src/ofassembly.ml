(* ============================================================
 * Copyright (c) 2017 tacigar. All rights reserved.
 * https://github.com/tacigar/oldfashion
 * ============================================================ *)

exception Invalid_syntax

type symbol =
  | Variable_symbol of string * int
  | Function_symbol of string * int
 and scope =
  | Scope of scope option * (symbol list) ref
            
class translator = object(self)
  val mutable buffer_ = Buffer.create 1000
  val mutable cur_label_ = 0
  val mutable cur_scope_ = Scope (None, ref [])
  val mutable cur_global_index_ = 0
  val mutable cur_frame_index_ = 0

  (* This method must be called after calling translate method *)
  method globals = cur_global_index_

  method cur_label =
    ".label " ^ (string_of_int cur_label_)

  method gen_label =
    cur_label_ <- cur_label_ + 1;
    self#cur_label

  (* Return value is tuple, first element is index of frame, 
     second element is whether that symbol is global. *)
  method lookup_symbol name =
    let rec lookup = function
      | Scope (parent, symbols) ->
         try
           let symbol =
             List.find (fun x ->
                 match x with
                 | Variable_symbol (s, n) ->
                    s = name
                 | _ -> false
               ) !symbols
           in
           let number =
             match symbol with
             | Variable_symbol (_, n) -> n
             | _ -> assert false
           in               
           match parent with
           | Some _ -> number, false (* it is not global symbol *)
           | None   -> number, true  (* it is global symbol *)
         with Not_found as e->
           match parent with
           | Some p -> lookup p
           | None -> raise e
    in
    lookup cur_scope_

  method translate program =
    List.iter (fun external_definition ->
        self#translate_external_definition external_definition
      ) program;
    Buffer.contents buffer_
                    
  method translate_external_definition = function
    | Ofast.Function_external_definition (funcname, params, body) ->
       self#translate_function_external_definition funcname params body
    | Ofast.Variable_external_definition (varname, exp_option) ->
       self#translate_variable_external_definition varname exp_option

  method translate_function_external_definition funcname params body =
    (* for variable declarations *)
    cur_frame_index_ <- 0;
    let origin = cur_scope_ in
    let new_scope = Scope (Some cur_scope_, ref []) in
    cur_scope_ <- new_scope;
    
    Buffer.add_string buffer_ ".def ";
    Buffer.add_string buffer_ funcname;
    Buffer.add_string buffer_ ": args=";
    Buffer.add_string buffer_ (string_of_int (List.length params));

    (* Register parameter list *)
    List.iter (fun param ->
        match cur_scope_ with
        | Scope (_, symbols) ->
           symbols := Variable_symbol (param, cur_frame_index_) :: !symbols;
           cur_frame_index_ <- cur_frame_index_ + 1
      ) params;
    
    (* Before writing about local variables, check statement and write tmp buf. *)
    let tmpbuf = Buffer.create 1000 in
    List.iter (fun stmt ->
        self#translate_statement tmpbuf stmt
      ) body;
    Buffer.add_string buffer_ ", locals=";
    Buffer.add_string buffer_ (string_of_int cur_frame_index_);
    Buffer.add_char buffer_ '\n';
    Buffer.add_buffer buffer_ tmpbuf;
    begin
      if funcname = "main" then
        Buffer.add_string buffer_ "HALT\n"
      else
        Buffer.add_string buffer_ "RET\n"
    end;
    cur_scope_ <- origin

  method translate_variable_external_definition varname exp_option =
    begin    
      match exp_option with
      | Some exp ->
         self#translate_expression buffer_ false exp
      | _ -> ()
    end;
    Buffer.add_string buffer_ "FGSTORE ";
    Buffer.add_string buffer_ (string_of_int cur_global_index_);
    Buffer.add_char buffer_ '\n';
    (* Update global scope and current global index. *)
    begin
      match cur_scope_ with
        Scope (_, symbols) ->
        symbols := Variable_symbol (varname, cur_global_index_) :: !symbols;
    end;
    cur_global_index_ <- cur_global_index_ + 1

  method translate_statement buffer = function
    | Ofast.If_statement (cond, tstmts, fstmts_option) ->
       self#translate_if_statement
         buffer cond tstmts fstmts_option
    | Ofast.For_statement (init_stmt, cond, prop_stmt, stmts) ->
       self#translate_for_statement
         buffer init_stmt cond prop_stmt stmts
    | Ofast.While_statement (cond, stmts) ->
       self#translate_while_statement buffer cond stmts
    | Ofast.Variable_declaration_statement (varname, exp_option) ->
       self#translate_variable_declaration_statement buffer varname exp_option
    | Ofast.Variable_assign_statement (varname, exp) ->
       self#translate_variable_assign_statement buffer varname exp
    | Ofast.Table_value_assign_statement (varname, key, value) ->
       self#translate_table_value_assign_statement buffer varname key value
    | Ofast.Function_call_statement (funcname, args) ->
       self#translate_function_call_statement buffer funcname args
    | Ofast.Return_statement (exp_option) ->
       self#translate_return_statement buffer exp_option

  (* Because all statements is in function scope,
     insert two space *)
  method write_statement buffer strs =
    Buffer.add_string buffer "  ";
    List.iter (fun str ->
        Buffer.add_string buffer str
      ) strs;
    Buffer.add_char buffer '\n'
                                       
  method translate_if_statement buffer cond tstmts fstmts_option =
    self#translate_expression buffer true cond;
    let new_label = self#gen_label in
    self#write_statement buffer ["TEST "; (string_of_int cur_label_)];
    List.iter (fun stmt ->
        self#translate_statement buffer stmt
      ) tstmts;
    Buffer.add_string buffer new_label;
    Buffer.add_char buffer '\n';
    match fstmts_option with
    | Some fstmts ->
       List.iter(fun stmt ->
           self#translate_statement buffer stmt
         ) fstmts
    | _ -> ()
             
  method translate_for_statement buffer init_stmt cond prop_stmt stmts =
    begin
      match init_stmt with
      | Ofast.Variable_assign_statement (varname, exp) ->
         self#translate_variable_assign_statement buffer varname exp 
      | _ -> raise Invalid_syntax
    end;
    let start_label = self#gen_label in
    let start_label_n = cur_label_ in
    let end_label = self#gen_label in
    let end_label_n = cur_label_ in
    Buffer.add_string buffer start_label;
    Buffer.add_char buffer '\n';
    self#translate_expression buffer true cond;
    self#write_statement buffer ["TEST "; (string_of_int end_label_n)];
    List.iter (fun stmt ->
        self#translate_statement buffer stmt
      ) stmts;
    self#translate_statement buffer prop_stmt;
    (* Go back to start label *)
    self#write_statement buffer ["JUMP "; (string_of_int start_label_n)];
    Buffer.add_string buffer end_label;
    Buffer.add_char buffer '\n'
      
  method translate_while_statement buffer cond stmts =
    let start_label = self#gen_label in
    let start_label_n = cur_label_ in
    let end_label = self#gen_label in
    let end_label_n = cur_label_ in
    self#translate_expression buffer true cond;
    Buffer.add_string buffer start_label;
    Buffer.add_char buffer '\n';
    self#write_statement buffer ["TEST "; (string_of_int end_label_n)];
    List.iter (fun stmt ->
        self#translate_statement buffer stmt
      ) stmts;
    (* Go back to start label *)
    self#write_statement buffer ["JUMP "; (string_of_int start_label_n)];
    Buffer.add_string buffer end_label;
    Buffer.add_char buffer '\n'

  method translate_variable_declaration_statement buffer varname exp_option =
    begin
      match cur_scope_ with
      | Scope (_, symbols) ->
         symbols := Variable_symbol (varname, cur_frame_index_) :: !symbols;
         cur_frame_index_ <- cur_frame_index_ + 1
    end;
    match exp_option with
    | Some exp ->
       self#translate_expression buffer true exp;
       self#write_statement buffer ["STORE "; (string_of_int (cur_frame_index_ - 1))]
    | _ -> ()

  method translate_variable_assign_statement buffer varname exp =
    self#translate_expression buffer true exp;
    let index, is_global = self#lookup_symbol varname in
    match is_global with
    | true ->
       self#write_statement buffer ["FGSTORE "; (string_of_int index)]
    | false ->
       self#write_statement buffer ["STORE "; (string_of_int index)]

  method translate_table_value_assign_statement buffer varname key value =
    self#translate_expression buffer true key;
    self#translate_expression buffer true value;
    let index, _ = self#lookup_symbol varname in
    self#write_statement buffer ["LOADTBL "; (string_of_int index)]

  method translate_function_call_statement buffer funcname args =
    List.iter (fun arg ->
        self#translate_expression buffer true arg
      ) args;
    self#write_statement buffer ["CALL "; funcname]

  method translate_return_statement buffer exp_option =
    match exp_option with
    | Some exp ->
       self#translate_expression buffer true exp;
       self#write_statement buffer ["RET 1"]
    | None ->
       self#write_statement buffer ["RET 0"]
      
  method translate_expression buffer in_func = function
    | Ofast.Variable_expression varname ->
       self#translate_variable_expression buffer in_func varname
    | Ofast.Boolean_literal_expression b ->
       self#translate_boolean_literal_expression buffer in_func b
    | Ofast.Numeric_literal_expression n ->
       self#translate_numeric_literal_expression buffer in_func n
    | Ofast.String_literal_expression s ->
       self#translate_string_literal_expression buffer in_func s
    | Ofast.Table_constructor_expression kvs ->
       self#translate_table_constructor_expression buffer in_func kvs
    | Ofast.Table_value_expression (varname, key) ->
       self#translate_table_value_expression buffer in_func varname key
    | Ofast.Unary_operation_expression (op, exp) ->
       self#translate_unary_operation_expression buffer in_func op exp
    | Ofast.Binary_operation_expression (lhs, op, rhs) ->
       self#translate_binary_operation_expression buffer in_func lhs op rhs
    | Ofast.Function_call_expression (funcname, args) ->
       self#translate_function_call_expression buffer in_func funcname args
    | Ofast.Paren_expression exp ->
       self#translate_expression buffer in_func exp

  method write_expression buffer in_func strs =
    if in_func then
      Buffer.add_string buffer "  ";
    List.iter (fun str ->
        Buffer.add_string buffer str
      ) strs;
    Buffer.add_char buffer '\n'
                            
  method translate_variable_expression buffer in_func varname =
    let index, is_global = self#lookup_symbol varname in
    match is_global with
    | true ->
       self#write_statement buffer ["FGLOAD "; (string_of_int index)]
    | false ->
       self#write_statement buffer ["LOAD "; (string_of_int index)]

  method translate_boolean_literal_expression buffer in_func b =
    self#write_expression buffer in_func ["CONST "; (string_of_bool b)]

  method translate_numeric_literal_expression buffer in_func n =
    self#write_expression buffer in_func ["CONST "; (string_of_float n)]

  method translate_string_literal_expression buffer in_func s =
    self#write_expression buffer in_func ["CONST "; "\""; s; "\""]

  method translate_table_constructor_expression buffer in_func kvs =
    List.iter (fun kv ->
        let k = fst kv in
        let v = snd kv in
        self#translate_expression buffer in_func k;
        self#translate_expression buffer in_func v
      ) kvs;
    let n = List.length kvs in
    self#write_expression buffer in_func ["NEWTBL "; (string_of_int n)]

  method translate_table_value_expression buffer in_func varname key =
    self#translate_expression buffer in_func key;
    let index, _ = self#lookup_symbol varname in
    self#write_expression buffer in_func ["STORETBL "; (string_of_int index)]

  method translate_unary_operation_expression buffer in_func op exp =
    self#translate_expression buffer in_func exp;
    match op with
    | Ofast.Uminus ->
       self#write_expression buffer in_func ["MINUS"]
    | Ofast.Uplus ->
       ()
      
  method translate_binary_operation_expression buffer in_func lhs op rhs =
    self#translate_expression buffer in_func lhs;
    self#translate_expression buffer in_func rhs;
    match op with
    | Ofast.Plus ->
       self#write_expression buffer in_func ["ADD"]
    | Ofast.Minus ->
       self#write_expression buffer in_func ["SUB"]
    | Ofast.Mult ->
       self#write_expression buffer in_func ["MUL"]
    | Ofast.Div ->
       self#write_expression buffer in_func ["DIV"]
    | Ofast.Lt ->
       self#write_expression buffer in_func ["LT"]
    | Ofast.Le ->
       self#write_expression buffer in_func ["LE"]
    | Ofast.Gt ->
       self#write_expression buffer in_func ["GT"]
    | Ofast.Ge ->
       self#write_expression buffer in_func ["GE"]
    | Ofast.Ne ->
       self#write_expression buffer in_func ["NE"]
    | Ofast.Eq ->
       self#write_expression buffer in_func ["EQ"]
    | Ofast.And ->
       self#write_expression buffer in_func ["AND"]
    | Ofast.Or ->
       self#write_expression buffer in_func ["OR"]

  method translate_function_call_expression buffer in_func funcname args =
    List.iter (fun arg ->
        self#translate_expression buffer in_func arg
      ) args;
    self#write_expression buffer in_func ["CALL "; funcname]
end
