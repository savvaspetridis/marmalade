(* open Sast *)
open Ast

let to_java marma =
	"import java.util.Arrays;\n" ^
	"import java.util.ArrayList;\n" ^ 
	"public class marmalade{\n" ^

	"\n\npublic static void main(String[] args) {\n" ^
		String.concat "\n" (List.map stmt_write marma.stmts) ^
		"\n}\n}"

(* let stmt_write = *)





(* Below: From Fry *)

let stmt_write = function
(*	S_Block(_,ss) -> "{\n" ^ String.concat "\n" ( List.rev (List.map j_stmt ss)) ^ "\n}"	
| 	S_Expr(e,_) -> j_expr e ^ ";"
|   S_Return(e,_) -> "return " ^ j_expr e ^";"
| 	S_If(elif_l, s) -> writeIf elif_l s 
| 	S_For(e1, e2, s) -> writeForLoop e1 e2 s
| 	S_While(e, s) -> writeWhileLoop e s *)
VarDecl(v) -> write_var_decl v
| Expr(e) -> write_expr e

let write_var_decl v = 
	match v with 
	Assign(id, exp) -> write_assign id exp
	| 

let write_assign i e = 
	match e with
	_ -> write_type e ^ " " ^ i ^ " = " ^ write_expr e


let write_expr e = 
	match e with
	IntLit(i) -> string_of_int i
	| StringLit(str) -> "\"" ^ str ^ "\""
	| Id(x) -> x
	| Binop(e_1, op, e_2) -> write_bin_op e_1 op e_2
	| 


let write_bin_op e1 op e2 = 









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






