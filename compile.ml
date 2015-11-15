(* open Sast *)
open Ast

let rec to_java marma =
	"import java.util.Arrays;\n" ^
	"import java.util.ArrayList;\n" ^ 
	"import jm.JMC;\n" ^
	"import jm.music.data.*;\n" ^
	"import jm.util.Play;\n" ^
	"public class marma implements JMC{\n" ^



	"\n\npublic static void main(String[] args) {\n" ^
		String.concat "\n" (List.map stmt_write marma.stmts) ^
		"\n}\n}" 

(* Below: From Fry *)

and stmt_write = function
(*	S_Block(_,ss) -> "{\n" ^ String.concat "\n" ( List.rev (List.map j_stmt ss)) ^ "\n}"	
| 	S_Expr(e,_) -> j_expr e ^ ";"
|   S_Return(e,_) -> "return " ^ j_expr e ^";"
| 	S_If(elif_l, s) -> writeIf elif_l s 
| 	S_For(e1, e2, s) -> writeForLoop e1 e2 s
| 	S_While(e, s) -> writeWhileLoop e s *)
VarDecl(v) -> write_var_decl v ^ ";\n"
| Expr(e) -> write_expr e ^ ";\n"

and write_var_decl v = 
	match v with 
	Assign(typ, id, exp) ->  write_assign id exp typ

and write_assign i e t = 
	match e with 
	_ -> write_type t ^ " " ^ i ^ " = " ^ write_expr e

and write_type ty =
	match ty with
	Int -> "int"
	| String -> "String"
	| Note -> "Note"
	| Measure -> "Note []"
	| Phrase -> "Note [][]"
	| Song -> "Note [][][]"
    | List -> "ArrayList<Our_Object>"

and write_expr e = 
	match e with
	IntLit(i) -> string_of_int i
	| String_Lit(str) -> "\"" ^ str ^ "\""
	| Id(x) -> x
(*	| Binop(e_1, op, e_2, t) -> writeBinOp e_1 op e_2 t *)
	| Binop(e_1, op, e_2) -> writeBinOp e_1 op e_2
	| BasicList(l) -> "{" ^ String.concat "," (List.map write_expr l) ^  "}"
	| Note(nt, dr) -> "new Note(" ^ string_of_int nt ^ ", " ^ write_rhythm dr  ^ ")"
	| FuncList(funk_args, l) -> "{" ^ String.concat "," (List.map write_expr l)
    ^ "};\n" ^ String.concat ";\n" (List.map2 mapcall funk_args l) ^ "int i = 0"
    | FunkCall(name, args) -> name ^ "(" ^ String.concat "," (List.map
    write_expr args) ^ ");\n"

and mapcall func param = 
	match func with 
    FunkCall(name, args) -> (match name with 
    "print" -> "System.out.println(" ^ (*String.concat "," (List.map write_expr args)*) write_expr param ^ ");\n" 
    | "play" -> "Play.midi(" ^ (*(String.concat "," (List.map write_expr args)*) write_expr param ^
    ");\n")

and write_rhythm dr =
	match dr with 
	's' -> "0.25"
	| 'e' -> "0.5"
    | 'q' -> "1.0"
	| 'h' -> "1.5"
	| 'w' -> "2.0"

(* This is a replacement from Fry *)
and writeBinOp e1 o e2 = 
	match o with 
	Equal -> write_expr e1 ^".equals("^ write_expr e2^") " 
	| Neq -> "!" ^ write_expr e1 ^".equals("^ write_expr e2^") " 
	| _ -> write_expr e1 ^ 
		(match o with Plus -> "+" | Minus -> "-" | Times -> "*" |
				  Divide -> "/" |  
				  Less -> "<" | Leq -> "<=" | Greater -> ">" |
				  Geq -> ">=" (* |  In -> "In" | Notin -> "Notin" | From -> "From" *)
				 (* | And -> "&&" | Or -> "||" *)) ^ write_expr e2
(*

	I don't think this will work becasue we don't have types in writeBinOp in AST

let writeBinOp ex1 op ex2 typ = 
	let e1 = write_expr ex1 and e2 = write_expr ex2 in
		let helper e1 op e2 = 
			match typ with
            Int -> (match op with (Plus | Minus | Times | Divide | Equal | Neq |
            Less | Leq | Greater | Geq) -> e1 ^ write_op_primitive op ^ e2)
            | Note -> e1 ^ ".setPitch( " ^ e1 ^ ".getPitch() + " ^ e2 ^
            ".getPitch());\n"
            Note n = new Note(C4+i, CROTCHET); *)

(* Functions below were copied from Corgi - Fall 2014 *)

and write_op_primitive = function
	Plus -> " + "
	| Minus -> " - "
	| Times -> " * "
	| Divide -> " / "
	| Equal -> " == "
	| Neq -> " != "
	| Less -> " < " 	
	| Leq -> " <= "
	| Greater -> " > "
	| Geq -> " >= "
	| _ -> raise (Failure "and/or begin applied to a java primitive")
(*
let write_op_compares e1 op e2 =
	match op with 
	Equal -> "(" ^ e1 ^ ").equals(" ^ e2 ^ ")"
	| Less ->  "(" ^ e1 ^ ").compareTo(" ^ e2 ^ ")" ^ " < 0"
	| Leq -> "(" ^ e1 ^ ").compareTo(" ^ e2 ^ ")" ^ " <= 0"
	| Greater -> "(" ^ e1 ^ ").compareTo(" ^ e2 ^ ")" ^ " > 0"
	| Geq -> "(" ^ e1 ^ ").compareTo(" ^ e2 ^ ")" ^ " >= 0"
	| Neq -> "(" ^ e1 ^ ").compareTo(" ^ e2 ^ ")" ^ " != 0"
	| _ -> raise (Failure "not a comparator operation")
*)

