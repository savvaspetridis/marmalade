(* open Sast *)
open Ast

let rec to_java = 
	"import java.util.Arrays;\n" ^
	"import java.util.ArrayList;\n" ^ 
	"public class marmalade{\n" ^

	"\n\npublic static void main(String[] args) {\n" ^
		String.concat "\n" ()

