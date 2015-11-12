open Printf

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  compiled_program = Compile.translate program
    in let file = open_out ("marma.java") in
      fprintf file "%s"  compiled_program; 