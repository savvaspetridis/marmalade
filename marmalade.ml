open Printf

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let compiled_program = Compile.to_java program in 
     let file = open_out ("marma.java") in
      fprintf file "%s"  compiled_program; 
