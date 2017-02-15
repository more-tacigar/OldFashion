(* ============================================================
 * Copyright (c) 2017 tacigar. All rights reserved.
 * https://github.com/tacigar/oldfashion
 * ============================================================ *)

class assembler = object(self)
  val mutable cur_index_consts = 0
  val mutable cur_address = 0
  val mutable consts : Ofobject.value list = []
  val mutable code : char list = []
  val label_tbl = Hashtbl.create 100
  val func_tbl = Hashtbl.create 100 

  (* This method returns a tuple, which elements are
   * 1. code
   * 2. const_pool
   *)
  method assemble (program : Ofassembler_ast.program) =
    List.iter (fun code ->
        match code with
        | Ofassembler_ast.Function_define (funcname, args, locals) ->
           let new_func = Ofobject.Function (funcname, args, locals, cur_address) in
           consts <- new_func :: consts
        | Ofassembler_ast.Label_define label_number ->
           Hashtbl.add label_tbl label_number cur_address
        | Ofassembler_ast.Instruction (instruction, arg_option) ->
           self#assemble_instruction instruction arg_option
      ) program;
    ((Array.of_list (List.rev consts)))

  method add_int_to_code n =
    code <- (char_of_int n) :: code

  method find_label_address n =
    Hashtbl.find label_tbl n

  method find_function_address funcname =
    Hashtbl.find func_tbl funcname
                                 
  method assemble_instruction instruction arg_option =
    let open Ofcode in
    match instruction with
    (* These instructions must have an int argument. *)
    | FGSTORE | FGLOAD | STORE | LOAD | STORETBL | LOADTBL | RET | NEWTBL ->
       begin
         match arg_option with
         | Some (Ofassembler_ast.Integer_literal n) -> self#add_int_to_code n
         | _ -> raise (Invalid_argument "must have int.")
       end
    (* These instructions must have a label number argument. *)
    | TEST | JUMP ->
       begin
         match arg_option with
         | Some (Ofassembler_ast.Integer_literal n) ->
            let adr = self#find_label_address n in
            self#add_int_to_code adr
         | _ -> raise (Invalid_argument "must have label number.")
       end
    (* This instruction must have a function name. *)
    | CALL ->
       begin
         match arg_option with
         | Some (Ofassembler_ast.Function_name funcname) ->
            let adr = self#find_function_address funcname in
            self#add_int_to_code adr
         | _ -> raise (Invalid_argument "must have function name.")
       end
    (* This instruction must have a constant value, ex: string, float, ... *)
    | CONST ->
       begin
         match arg_option with
         | Some arg ->
            begin
              match arg with
              | Ofassembler_ast.String_literal s -> () (*Ofobject.String s*)
              | Ofassembler_ast.Integer_literal n -> () (*Ofobject.Number (float_of_int n)*)
              | Ofassembler_ast.Numeric_literal n -> () (*Ofobject.Number n *)
              | _ -> raise (Invalid_argument "function is invalid.")
            end
         | None -> raise (Invalid_argument "must have argument.")
       end
    (* These instructions must not have any arguments. *)
    | Ofcode.MINUS | Ofcode.ADD | Ofcode.SUB | Ofcode.MUL | Ofcode.DIV | Ofcode.LT | Ofcode.LE
      | Ofcode.GT | Ofcode.GE | Ofcode.NE | Ofcode.EQ | Ofcode.AND | Ofcode.OR ->
       begin
         match arg_option with
         | Some arg -> raise (Invalid_argument "must have nothing.")
         | None -> ()
       end
end
