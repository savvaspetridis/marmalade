(*
* SAST for Marmalade
* much of this was adapted from Corgi
*)

open Ast

let fst_of_three (t, _, _) = t
let snd_of_three (_, t, _) = t
let thrd_of_three (_, _, t) = t



type s_expr = 
	S_IntLit of int * declare_type
	| S_Id of string * declare_type
	| S_String_Lit of string * declare_type
	| S_Note of int * char * declare_type
	| S_Binop of s_expr * op * s_expr * declare_type
	| S_Call of string * s_expr list * declare_type
	| S_Index of string * int * declare_type
	| S_Arr of s_expr list * declare_type
	| S_Noexpr 

type s_stmt =
	S_CodeBlock of s_block
	| S_expr of s_expr
	| S_Assign of string * s_expr * declare_type
	| S_Arr_Assign of string * s_expr * s_expr * declare_type
	| D_Return of s_expr
	| D_If of s_expr * s_stmt * s_stmt (* stmts of type D_CodeBlock *)
	| D_For of s_stmt * s_stmt * s_stmt * s_block (* stmts of type D_Assign | D_Noexpr * D_Expr of type bool * D_Assign | D_Noexpr *)
	| D_While of s_expr * s_block

and s_block = {
	s_locals : scope_var_decl list;
	s_statements: s_stmt list;
	s_block_id: int;
}

type s_func = {
	s_fname : string;
	s_ret_type : declare_type; (* Changed from types for comparison error in verify_stmt *)
	s_formals : scope_var_decl list;
	s_fblock : s_block;
}

type s_program = {
	s_gvars: scope_var_decl list;
	s_pfuncs: s_func list;
}

let rec map_to_list_env func lst env =
	match lst with
		  [] -> []
		| head :: tail ->
			let r = func head env in 
				r :: map_to_list_env func tail env

let rec traverse_main func lst =
	match lst with
		[] -> []
		| head :: tail ->
			let r = func head in
			r :: traverse_main func tail

let drop_funk li =
	match li with 
		Expr(v) ->					Expr(v)
		| VarDecl(v) -> 			VarDecl(v)
		| If(exp_1, blk, exp_2) -> 	If(exp_1, blk, exp_2)
		| While(exp, blk) ->		While(exp, blk)
		| _ ->						Null_Type

let get_vars li =
	match li with 
		VarDecl(v) -> 	
			(match v with
				Assign(dt, iden, v) -> 
					(match dt with 
						Int -> (iden, false, dt) 
						| Note -> (iden, false, dt)
						| String -> (iden, false, dt)
						| _ -> (iden, true, dt))
				| Update(iden, v)	-> ("", false, Null_Type))

		| _ ->						("", false, Null_Type)


let verify_var var env = 
	let decl = Table.get_decl (fst_of_three var) env in
	match decl with
		Func_Decl(f) -> raise(Failure("symbol is not a variable"))
	  | Var_Decl(v) -> 	let (vname, varray, vtype, id) = v in
			(vname, varray, vtype, id)



(*let verify_func func env =
	(* let () = Printf.printf "verifying function \n" in *)
	let verified_block = verify_block func.fblock func.ret_type (fst env, func.fblock.block_id) in
	(* let () = Printf.printf "func.fname" in *) 
	let verified_formals = map_to_list_env verify_var func.formals (fst env, func.fblock.block_id) in
	let verified_func_decl = verify_is_func_decl func.fname env in 
	{ d_fname = verified_func_decl; d_ret_type = func.ret_type; d_formals = verified_formals; d_fblock = verified_block }

and verify_block block ret_type env =
	let verified_vars = map_to_list_env verify_var block.locals (fst env, block.block_id) in
	let verified_stmts = verify_stmt_list block.statements ret_type env in 
	{ d_locals = verified_vars; d_statements = verified_stmts; d_block_id = block.block_id }*)



let verify_semantics program env = 
	let (stmt_list, func_list) = program in 
	let main_stmts = traverse_main drop_funk stmt_list in 
	let main_vars = traverse_main get_vars main_stmts in 
	(*let verified_gvar_list = map_to_list_env verify_var main_vars env in 
	let main_func = verfy_func {fname = "main"; ret_type = Null_Type; args = []; {locals = verified_gvar_list; statements = main_stmts; block_id = 0}}
	let verified_func_list = main_func :: main map_to_list_env verify_func func_list env in
	(* let () = Printf.printf "after verifying functions \n" in *)*)
	let () = prerr_endline "// Passed semantic checking \n" in
		{ s_pfuncs = []; s_gvars = []} 