(* ============================================================
 * Copyright (c) 2017 tacigar. All rights reserved.
 * https://github.com/tacigar/oldfashion
 * ============================================================ *)

class vm globals init_consts init_code start_adr start_locals = object(self)
  val mutable ip_register = start_adr
  val mutable fp_register = 0
  val mutable sp_register = 0
  val mutable code = init_code
  val mutable globals = Array.make globals (Ofobject.String "")
  val mutable consts = init_consts
  val mutable call_stack = Stack.create ()
  val mutable frame_stack = Stack.create ()
  val mutable operands = Stack.create ()

  initializer
    Stack.push (Array.make start_locals (Ofobject.String "")) frame_stack
                                      
  method dump_consts =
    Array.iter (fun c ->
        match c with
        | Ofobject.String s -> Printf.printf "S : %s\n" s
        | Ofobject.Number n -> Printf.printf "N : %f\n" n
        | Ofobject.Function (funcname, args, locals, adr) ->
           Printf.printf "F : %s %d %d %d\n" funcname args locals adr
        | _ -> ()
      ) consts
                                      
  method code_to_int i =
    let w1 = (int_of_char code.(i)) lsl 32 in
    let w2 = (int_of_char code.(i+1)) lsl 16 in
    let w3 = (int_of_char code.(i+2)) lsl 8 in
    let w4 = (int_of_char code.(i+3)) in
    let res = w1 lor w2 lor w3 lor w4 in
    res
                                    
  method execute =
    while ip_register < (Array.length code) do
      let open Ofcode in
      begin
        match Ofcode.of_int (int_of_char (code.(ip_register))) with
        | FGSTORE ->
           let n = self#code_to_int (ip_register + 1) in
           let v = Stack.pop operands in
           globals.(n) <- v;
           ip_register <- ip_register + 4
        | FGLOAD ->
           let n = self#code_to_int (ip_register + 1) in
           Stack.push globals.(n) operands;
           ip_register <- ip_register + 4
        | TEST ->
           let b = Stack.pop operands in
           begin
             match b with
             | Ofobject.Bool b ->
                if b then
                  let n = self#code_to_int (ip_register + 1) in
                  ip_register <- n
             | _ -> ()
           end
        | JUMP ->
           let n = self#code_to_int (ip_register + 1) in
           ip_register <- n
        | STORE ->
           let n = self#code_to_int (ip_register + 1) in
           let frame = Stack.top frame_stack in
           let v = Stack.pop operands in

           frame.(n) <- v;
           ip_register <- ip_register + 4
        | LOAD ->
           let n = self#code_to_int (ip_register + 1) in
           let frame = Stack.top frame_stack in
           Stack.push frame.(n) operands;
           ip_register <- ip_register + 4
        | STORETBL -> ()
        | LOADTBL -> ()
        | CALL -> ()
        | RET -> ()
        | CONST ->
           let n = self#code_to_int (ip_register + 1) in
           let v = consts.(n) in
           Stack.push v operands;
           ip_register <- ip_register + 4
        | NEWTBL -> ()
        | MINUS ->
           let operand = Stack.pop operands in
           begin
             match operand with
             | Ofobject.Number n ->
                let res = Ofobject.Number (-. n) in
                Stack.push res operands
             | _ ->
                raise (Invalid_argument "invalid")
           end
        | ADD ->
           self#binary_number_operation1 (+.)
        | SUB ->
           self#binary_number_operation1 (-.)
        | MUL -> 
           self#binary_number_operation1 ( *. )
        | DIV -> 
           self#binary_number_operation1 (/.)
        | LT ->
           self#binary_number_operation2 (<)
        | LE -> 
           self#binary_number_operation2 (<=)
        | GT -> 
           self#binary_number_operation2 (>)
        | GE ->
           self#binary_number_operation2 (>=)
        | NE ->
           self#binary_number_operation2 (<>)
        | EQ -> 
           self#binary_number_operation2 (=)
        | AND ->
           self#binary_logical_operation (&&)
        | OR ->
           self#binary_logical_operation (||)
        | HALT -> ()
      end;
      ip_register <- ip_register + 1
    done;
    ()

  method binary_number_operation1 op =
    let rhs = Stack.pop operands in
    let lhs = Stack.pop operands in
    begin
      match rhs, lhs with
      | Ofobject.Number r, Ofobject.Number l ->
         let res = Ofobject.Number (op r l) in
         Stack.push res operands
      | _, _ ->
         raise (Invalid_argument "invalid")
    end

  method binary_number_operation2 op =
    let rhs = Stack.pop operands in
    let lhs = Stack.pop operands in
    begin
      match rhs, lhs with
      | Ofobject.Number r, Ofobject.Number l ->
         let res = Ofobject.Bool (op r l) in
         Stack.push res operands
      | _, _ ->
         raise (Invalid_argument "invalid")
    end

  method binary_logical_operation op =
    let rhs = Stack.pop operands in
    let lhs = Stack.pop operands in
    begin
      match rhs, lhs with
      | Ofobject.Bool r, Ofobject.Bool l ->
         let res = Ofobject.Bool (op r l) in
         Stack.push res operands
      | _, _ ->
         raise (Invalid_argument "invalid")
    end
end
