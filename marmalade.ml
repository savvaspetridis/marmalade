open Printf

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let env = Table.build_table program in
  let compiled_program = Compile.to_java program Sys.argv.(1) in 
     let file = open_out (Sys.argv.(1) ^ ".java") in
      fprintf file "%s"  compiled_program; 
