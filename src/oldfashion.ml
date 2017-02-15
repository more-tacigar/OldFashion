(* ============================================================
 * Copyright (c) 2017 tacigar. All rights reserved.
 * https://github.com/tacigar/oldfashion
 * ============================================================ *)

let usage_msg = "USAGE : oldfashion [options] filename"

let dump_ast filename =
  let lexbuf = Lexing.from_channel (open_in filename) in
  let ast = Ofparser.program Oflexer.read lexbuf in
  let ast_str = Ofast.to_string ast in
  print_string ast_str

let dump_asm filename =
  let lexbuf = Lexing.from_channel (open_in filename) in
  let ast = Ofparser.program Oflexer.read lexbuf in
  let translator = new Ofassembly.translator in
  let asm = translator#translate ast in
  print_string asm

let dump_asmast filename =
  let lexbuf = Lexing.from_channel (open_in filename) in
  let ast = Ofparser.program Oflexer.read lexbuf in
  let translator = new Ofassembly.translator in
  let asm = translator#translate ast in
  let lexbuf = Lexing.from_string asm in
  let ast = Ofassembler_parser.program Ofassembler_lexer.read lexbuf in
  let ast_str = Ofassembler_ast.to_string ast in
  print_string ast_str

let execute filename =
  let lexbuf = Lexing.from_channel (open_in filename) in
  let ast = Ofparser.program Oflexer.read lexbuf in
  let translator = new Ofassembly.translator in
  let asm = translator#translate ast in
  let lexbuf = Lexing.from_string asm in
  let ast = Ofassembler_parser.program Ofassembler_lexer.read lexbuf in
  let assembler = new Ofassembler.assembler in
  let consts, code, start_adr, start_locals = assembler#assemble ast in
  let vm = new Ofvm.vm translator#globals consts code start_adr start_locals in
  vm#dump_consts;
  vm#execute
               
let main () =
  let filename = ref "" in
  let ast_flag = ref false in
  let asm_flag = ref false in
  let asmast_flag = ref false in
  let spec_list = [
      ("-fdump-ast", Arg.Set ast_flag, "Dump the abstruct syntax tree.");
      ("-fdump-asm", Arg.Set asm_flag, "Dump the assembly representation.");
      ("-fdump-asmast", Arg.Set asmast_flag, "Dump the asemmbly ast.");
    ] in
  Arg.parse spec_list (fun str -> filename := str) usage_msg;
  if !filename = "" then
    print_endline usage_msg
  else if !ast_flag then
    dump_ast !filename
  else if !asm_flag then
    dump_asm !filename
  else if !asmast_flag then
    dump_asmast !filename
  else
    execute !filename

let () = main ()
