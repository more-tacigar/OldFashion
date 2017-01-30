
let main () =
  let filename = Sys.argv.(1) in
  let lexbuf = Lexing.from_channel (open_in filename) in
  let program = Ofparser.program Oflexer.read lexbuf in
  let result = Ofsyntax.Ast.to_string program in
  print_string result

let () = main ()
