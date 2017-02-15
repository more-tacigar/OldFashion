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
   * (const_pool, code, start_adr, start_locals)
   *)
  method assemble (program : Ofassembler_ast.program) =
    let start_adr = ref 0 in
    let start_locals = ref 0 in
    List.iter (fun code ->
        match code with
        | Ofassembler_ast.Function_define (funcname, args, locals) ->
           let new_func = Ofobject.Function (funcname, args, locals, cur_address) in
           Hashtbl.add func_tbl funcname cur_address;
           consts <- new_func :: consts;
           if funcname = "main" then
             begin
               start_adr := cur_address;
               start_locals := locals;
             end
        | Ofassembler_ast.Label_define label_number ->
           Hashtbl.add label_tbl label_number cur_address
        | Ofassembler_ast.Instruction (instruction, arg_option) ->
           self#assemble_instruction instruction arg_option
      ) program;
    ((Array.of_list (List.rev consts)), Array.of_list (List.rev code),
     !start_adr, !start_locals)
      
  method add_int_to_code n =
    let e1 = char_of_int ((n land 0xff000000) lsr 32) in
    let e2 = char_of_int ((n land 0x00ff0000) lsr 16) in
    let e3 = char_of_int ((n land 0x0000ff00) lsr 8) in
    let e4 = char_of_int n in
    code <- e1 :: code;
    code <- e2 :: code;
    code <- e3 :: code;
    code <- e4 :: code

  method find_label_address n =
    Hashtbl.find label_tbl n

  method find_function_address funcname =
    Hashtbl.find func_tbl funcname
                                 
  method assemble_instruction instruction arg_option =
    let open Ofcode in
    code <- (char_of_int (Ofcode.to_int instruction)) :: code;
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
              | Ofassembler_ast.String_literal s ->
                 consts <- Ofobject.String s :: consts;
                 self#add_int_to_code cur_index_consts;
                 cur_index_consts <- cur_index_consts + 1
              | Ofassembler_ast.Integer_literal n ->
                 consts <- Ofobject.Number (float_of_int n) :: consts;
                 self#add_int_to_code cur_index_consts;
                 cur_index_consts <- cur_index_consts + 1
              | Ofassembler_ast.Numeric_literal n ->
                 consts <- Ofobject.Number n :: consts;
                 self#add_int_to_code cur_index_consts;
                 cur_index_consts <- cur_index_consts + 1
              | _ -> raise (Invalid_argument "function is invalid.")
            end
         | None -> raise (Invalid_argument "must have argument.")
       end
    (* These instructions must not have any arguments. *)
    | Ofcode.MINUS | Ofcode.ADD | Ofcode.SUB | Ofcode.MUL | Ofcode.DIV
      | Ofcode.LT | Ofcode.LE | Ofcode.GT | Ofcode.GE | Ofcode.NE
      | Ofcode.EQ | Ofcode.AND | Ofcode.OR | Ofcode.HALT ->
       begin
         match arg_option with
         | Some arg -> raise (Invalid_argument "must have nothing.")
         | None -> ()
       end
end
