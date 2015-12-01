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

let write_op_primitive = function
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

let write_rhythm dr =
	match dr with 
	's' -> "0.25"
	| 'e' -> "0.5"
    | 'q' -> "1.0"
	| 'h' -> "1.5"
	| 'w' -> "2.0"

let write_op_primitive = function
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
	(*| Mod -> " % " *)
	| _ -> raise (Failure "and/or begin applied to a java primitive")

let rec get_typeof_dexpr = function
	 S_Int_Lit(intLit, t) -> t
	| S_String_Lit(strLit, t) -> t
	| S_Id (str, t) -> t
	| S_Arr(dexpr_list, t) -> t
	| S_Binop (dexpr1, op, dexpr2, t) -> t
	(* | D_Null_Lit -> "null" *)
	| S_Noexpr -> Null_Type
	| S_Call(str, _, dexpr_list, _, t) -> t


let write_op_compares e1 op e2 =
	match op with 
	Equal -> "(" ^ e1 ^ ").equals(" ^ e2 ^ ")"
	| Less ->  "(" ^ e1 ^ ").compareTo(" ^ e2 ^ ")" ^ " < 0"
	| Leq -> "(" ^ e1 ^ ").compareTo(" ^ e2 ^ ")" ^ " <= 0"
	| Greater -> "(" ^ e1 ^ ").compareTo(" ^ e2 ^ ")" ^ " > 0"
	| Geq -> "(" ^ e1 ^ ").compareTo(" ^ e2 ^ ")" ^ " >= 0"
	| Neq -> "(" ^ e1 ^ ").compareTo(" ^ e2 ^ ")" ^ " != 0"
	| _ -> raise (Failure("not a comparator operation"))



let rec write_expr = function
	S_Int_Lit(intLit, t) -> string_of_int intLit
	| S_String_Lit(strLit, t) -> "\"" ^ strLit ^ "\""
	| S_Id (str, yt) -> str
	| S_Arr(dexpr_list, t) -> write_array_expr dexpr_list t
	| S_Binop (dexpr1, op, dexpr2, t) -> write_binop_expr dexpr1 op dexpr2 t
	| S_Db_Arr(call, mark) -> (
			match mark with
			S_Arr(l_one, l_two) ->   write_expr call (*^ write_expr mark*) 
			| S_Noexpr -> write_expr call)
	| S_Noexpr -> ""
	| S_Note(i, ch, tp) -> string_of_int i ^ ", " ^ write_rhythm ch 
	| S_Call(str, exp, dexpr_list,t_ret, t_send) -> (match str with 
								  "print" -> "System.out.println("  ^ write_expr exp ^ ");\n"
								  | "play" -> "Play.midi(" ^ (*(String.concat "," (List.map write_expr args)*) write_expr exp ^ ");\n"
								  | "write" -> "Write.midi(" ^ (*(String.concat "," (List.map write_expr args)*) write_expr exp ^ ", \"out.mid\");\n"						 
								  | _ -> write_expr exp ^ "." ^ str ^ "(" ^ String.concat "," (List.map write_expr dexpr_list) ^ ");/n")
	| S_Call_lst(s) -> String.concat "" (List.map write_expr s)
	| _ -> raise(Failure(" is not a valid expression"))

and write_stmt (d:s_stmt) = match d with
	  S_CodeBlock(dblock) -> write_block dblock 
	| S_expr(dexpr) -> write_expr dexpr ^ ";"
	| S_Assign (name, dexpr, t) -> (
		match dexpr with
		S_Db_Arr(a1, a2) -> write_expr (S_Db_Arr(a1, a2)) ^ write_assign name a2 t ^ ";\n"
		| _ -> write_assign name dexpr t ^ ";\n" )
	| S_Return(dexpr) -> "return " ^ write_expr dexpr ^ ";\n"
    | S_If(dexpr, dstmt1, dstmt2) -> "if(" ^ write_expr dexpr ^  ")" ^  write_stmt dstmt1 ^ "else"  ^ write_stmt dstmt2
    | S_While(dexpr, dblock) -> "while(" ^ write_expr dexpr ^ ")"  ^ write_block dblock
    (*| S_Array_Assign(str,dexpr_value, dexpr_index, t) -> str ^ ".set(" ^ write_expr dexpr_index ^ "," ^ write_expr dexpr_value ^ ");"*)
    | _ -> raise(Failure(" is not a valid statement"))

and write_binop_expr expr1 op expr2 t =
	let e1 = write_expr expr1 and e2 = write_expr expr2 in 
		let write_binop_expr_help e1 op e2 = 
			match t with
				Int -> (match op with 
					(Plus | Minus | Times | Divide | Equal | Neq | Less | Leq | Greater | Geq | And | Or) ->  
					e1 ^ write_op_primitive op ^ e2)
			  | String -> (match op with 
					 Plus -> " + "
					| (Equal | Less | Leq | Greater | Geq) -> write_op_compares e1 op e2
					| _ -> raise(Failure(write_op_primitive op ^ " is not a supported operation for String_Type")))
			  | _ -> raise(Failure(write_op_primitive op ^ " is not a supported operation for" ^ write_type t))
		in write_binop_expr_help e1 op e2 

and write_array_expr dexpr_list t =
	  match t with
	 (*) Int -> "[" ^ String.concat "," (List.map write_expr dexpr_list) ^ "]"
	 |*) _ -> "new " ^ write_type t ^ " []"  ^ " {" ^ String.concat "," (List.map write_expr dexpr_list) ^ "}"


and tostring_str dexpr =
	let t = get_typeof_dexpr dexpr in
	match t with  
		 Int -> write_expr dexpr
		| String -> write_expr dexpr
		| _ -> "(" ^ write_expr dexpr ^ ").toString()"



and write_scope_var_decl_func svd =
	let (n, b, t, _) = svd in 
		write_type t ^ " " ^ n

and write_scope_var_decl svd =
	write_scope_var_decl_func svd ^ ";\n"

and write_global_scope_var_decl gsvd = 
	(*let () = Printf.printf "writing global \n" in*) 
	"static " ^ write_scope_var_decl_func gsvd ^ ";\n"

and write_assign name dexpr t =
	(match t with
	  Int | String | Intlist  -> name ^ " = " ^ write_expr dexpr
	| Note | Measure | Phrase | Song | Stringlist -> name ^ " = new " ^ write_type t ^ "(" ^ write_expr dexpr ^ ")"
	| _ -> raise(Failure(write_type t ^ " is not a valid assign_type")))



and write_block dblock =
	"{\n" ^  String.concat "\n" (List.map write_scope_var_decl dblock.s_locals) ^ String.concat "\n" (List.map write_stmt dblock.s_statements) ^ "\n}"

let write_func dfunc =
	match dfunc.s_fname with
	"main" -> "public static void main(String[] args)" ^ write_block dfunc.s_fblock
	| _ -> "static " ^ write_type dfunc.s_ret_type ^ " " ^ dfunc.s_fname ^ "("  ^ String.concat "," (List.map write_scope_var_decl_func dfunc.s_formals) ^ ")" ^ write_block dfunc.s_fblock


let gen_pgm pgm name = 
	(*let () = Printf.printf "got through table and sast \n" in *)
	"import java.util.Arrays;\n" ^
	"import java.util.ArrayList;\n" ^ 
	"import jm.JMC;\n" ^
	"import jm.music.data.*;\n" ^
	"import jm.util.*;\n" ^
	"public class " ^ name ^ " implements JMC{\n" ^String.concat "\n" (List.map write_global_scope_var_decl pgm.s_gvars) ^ String.concat "\n" (List.map write_func pgm.s_pfuncs) ^  "}"
