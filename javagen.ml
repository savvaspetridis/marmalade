open Ast
open Sast

let write_type = function 
	| Int -> "int"
	| String -> "String"
	| Note -> "Note"
	| Measure -> "Note[]"
	| Phrase -> "Note[][]"
	| Song -> "Note[][][]"
	| Intlist -> "int[]"
	| Stringlist -> "String[]"
	| _ -> raise(Failure "Type string of PD_Tuple or Null_Type being generated")

let write_scope_var_decl_func svd =
	let (n, b, t, _) = svd in 
		write_type t ^ " " ^ n

let write_scope_var_decl svd =
	write_scope_var_decl_func svd ^ ";\n"

let write_global_scope_var_decl gsvd = 
	let () = Printf.printf "writing global \n" in 
	"static " ^ write_scope_var_decl_func gsvd ^ ";\n"


let gen_pgm pgm name = 
	let () = Printf.printf "got through table and sast \n" in 
	"import java.util.Arrays;\n" ^
	"import java.util.ArrayList;\n" ^ 
	"import jm.JMC;\n" ^
	"import jm.music.data.*;\n" ^
	"import jm.util.*;\n" ^
	"public class " ^ name ^ " implements JMC{\n" ^String.concat "\n" (List.map write_global_scope_var_decl pgm.s_gvars) ^ (*String.concat "\n" (List.map write_func pgm.s_pfuncs) ^ *) "}"
