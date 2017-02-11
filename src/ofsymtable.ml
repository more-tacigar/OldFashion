(* ============================================================
 * Copyright (c) 2017 tacigar. All rights reserved.
 * https://github.com/tacigar/oldfashion
 * ============================================================ *)

type symbol =
  (* symbol name * storage index *)
  | Variable_symbol of string * int
  (* symbol name * scope * function number *)
  | Function_symbol of string * scope * int
 and scope =
  | Scope of (scope option) * ((symbol list) ref) * int ref

class symbol (name : string) = object(self)
  method name = name
end

and scope (es : scope option) = object(self)
  val enclosing_scope = es
  val local_variables : variable_symbol list ref = ref []
end
                                    
and variable_symbol (name : string) = object(self)
  inherit symbol name
end

and function_symbol (name : string) = object(self)
  inherit symbol name


end
                                                        
exception Redifinition_error
                                         
class symtable = object(self)
  val mutable current_scope = Scope (None, ref [], ref 0)
  val mutable current_funcnum = 0
                                    
  method private current_regnum =
    match current_scope with
    | Scope (_, _, regnum) -> regnum

  method private add_symbol new_symbol =
    match current_scope with
    | Scope (_, symbols, _) ->
       symbols := new_symbol :: !symbols
                                
  method add_variable varname =
    match current_scope with
    | Scope (_, symbols, regnum) ->
       let new_symbol = Variable_symbol (varname, !regnum) in
       symbols := new_symbol :: !symbols;
       regnum := !regnum + 1

  method add_function funcname =
    let new_scope = Scope (Some current_scope, ref [], ref 0) in
    let func_symbol = Function_symbol (funcname, new_scope, current_funcnum) in
    self#add_symbol func_symbol;
    current_funcnum <- current_funcnum + 1

                                    
end
  



