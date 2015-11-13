(* open Sast *)
open Ast

let to_java marma =
	"import java.util.Arrays;\n" ^
	"import java.util.ArrayList;\n" ^ 
	"public class marmalade{\n" ^

	"\n\npublic static void main(String[] args) {\n" ^
		String.concat "\n" (List.map stmt_write marma.stmts) ^
		"\n}\n}"

let stmt_write = 

(* Functions below were copied from Corgi - Fall 2014 *)

let write_op_primitive = function
	Add -> " + "
	| Sub -> " - "
	| Mult -> " * "
	| Div -> " / "
	| Equal -> " == "
	| Neq -> " != "
	| Less -> " < " 	
	| Leq -> " <= "
	| Greater -> " > "
	| Geq -> " >= "
	| Mod -> " % "
	| _ -> raise (Failure "and/or begin applied to a java primitive")

let write_op_compares e1 op e2 =
	match op with 
	Equal -> "(" ^ e1 ^ ").equals(" ^ e2 ^ ")"
	| Less ->  "(" ^ e1 ^ ").compareTo(" ^ e2 ^ ")" ^ " < 0"
	| Leq -> "(" ^ e1 ^ ").compareTo(" ^ e2 ^ ")" ^ " <= 0"
	| Greater -> "(" ^ e1 ^ ").compareTo(" ^ e2 ^ ")" ^ " > 0"
	| Geq -> "(" ^ e1 ^ ").compareTo(" ^ e2 ^ ")" ^ " >= 0"
	| Neq -> "(" ^ e1 ^ ").compareTo(" ^ e2 ^ ")" ^ " != 0"
	| _ -> raise (Failure "not a comparator operation")





(* let write_assign *)






