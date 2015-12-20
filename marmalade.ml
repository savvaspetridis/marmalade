(*
 * Compiler for Marmalade
 *)

open Printf

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let env = Table.build_table program in
  let sast_pgm = Sast.confirm_semantics program env in
  let compiled_program = (*Compile.to_java program Sys.argv.(1)*) Javagen.gen_pgm sast_pgm Sys.argv.(1) in 
     let file = open_out (Sys.argv.(1) ^ ".java") in
      fprintf file "%s"  compiled_program; 
