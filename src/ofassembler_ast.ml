(* ============================================================
 * Copyright (c) 2017 tacigar. All rights reserved.
 * https://github.com/tacigar/oldfashion
 * ============================================================ *)

type identifier = string
 and program = code list
 and code =
   | Function_define of identifier * int * int
   | Label_define of int
   | Instruction of identifier * argument option
 and argument =
   | String_literal of string
   | Integer_literal of int
   | Numeric_literal of float
   | Function_name of identifier
                        
let to_string program =
  let rec write_line buf depth str =
    for i = 0 to depth do
      Buffer.add_string buf "  "
    done;
    Buffer.add_string buf str;
    Buffer.add_char buf '\n'

  and translate_code buf depth = function
    | Function_define (funcname, args, locals) ->
       write_line buf depth "function define:";
       write_line buf (depth + 1) "function name:";
       write_line buf (depth + 2) funcname;
       write_line buf (depth + 1) "args:";
       write_line buf (depth + 2) (string_of_int args);
       write_line buf (depth + 1) "locals:";
       write_line buf (depth + 2) (string_of_int locals)
    | Label_define n ->
       write_line buf depth "label define:";
       write_line buf (depth + 1) "label number:";
       write_line buf (depth + 2) (string_of_int n)
    | Instruction (instruction, arg_option) ->
       write_line buf depth "instruction:";
       write_line buf (depth + 1) "instruction name:";
       write_line buf (depth + 2) instruction;
       match arg_option with
       | Some arg ->
          write_line buf (depth + 1) "argument:";
          translate_argument buf (depth + 2) arg
       | None ->
          ()

  and translate_argument buf depth = function
    | String_literal s ->
       write_line buf depth "string literal:";
       write_line buf (depth + 1) s
    | Integer_literal n ->
       write_line buf depth "integer literal:";
       write_line buf (depth + 1) (string_of_int n)
    | Numeric_literal n ->
       write_line buf depth "numeric literal:";
       write_line buf (depth + 1) (string_of_float n)
    | Function_name funcname ->
       write_line buf depth "function name:";
       write_line buf (depth + 1) funcname

  in

  let buf = Buffer.create 100 in
  List.iter (fun code ->
      translate_code buf 0 code
    ) program;
  Buffer.contents buf
