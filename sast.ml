(*
 * SAST for Marmalade
 * much of this was adapted from Corgi
 *)

open Ast

let fst_of_three (t, _, _) = t
let snd_of_three (_, t, _) = t
let thrd_of_three (_, _, t) = t

(* verified expressions *)

type s_expr = 
	S_Int_Lit of int * declare_type
	| S_Id of string * declare_type
	| S_String_Lit of string * declare_type
	| S_Note of int * char * declare_type
	| S_Measure of s_expr list * s_expr * declare_type (* S_Note list, S_TimeSig, declare_type *)
	| S_Phrase of s_expr list * s_expr * declare_type  (* S_Measure list, S_Instr, declare_type *)
	| S_Song of s_expr list  * s_expr * declare_type   (* S_Phrase list, *)
    | S_TimeSig of int * int * declare_type 		   (* ex: ((4:4), TimeSig) *)
    | S_Instr of string * declare_type 				   (* ex: (BASS, Instr) *)
    | S_Tempo of int * declare_type					   (* ex: (120, Tempo) *)
    | S_Binop of s_expr * op * s_expr * declare_type
	| S_Call of string * s_expr * s_expr list * declare_type list * declare_type
	| S_Index of string * s_expr * declare_type
	| S_Arr of s_expr list * declare_type
	| S_Db_Arr of s_expr * s_expr
	| S_Call_lst of s_expr list
	| S_Noexpr											(* Default - No value *)

(* verified statemnets *)	

type s_stmt =
	S_CodeBlock of s_block
	| S_expr of s_expr
	| S_Assign of string * s_expr * declare_type
	| S_Arr_Assign of string * s_expr * s_expr * declare_type
	| S_Return of s_expr
	| S_If of s_expr * s_stmt * s_stmt (* stmts of type D_CodeBlock *)
	| S_For of s_stmt * s_stmt * s_stmt * s_block (* stmts of type D_Assign | D_Noexpr * D_Expr of type bool * D_Assign | D_Noexpr *)
	| S_While of s_expr * s_block
	| S_Append_Assign of declare_type * string * s_expr list
	| S_Index_Update of string * s_expr * s_expr * declare_type


and s_block = {
	s_locals : scope_var_decl list;
	s_statements: s_stmt list;
	s_block_id: int;
}

(* verified function declaration *)

type s_func = {
	s_fname : string;
	s_ret_type : declare_type; (* Changed from types for comparison error in confirm_stmt *)
    s_f_type : declare_type list;
    s_formals : scope_var_decl list;
	s_fblock : s_block;
}

type s_program = {
	s_gvars: scope_var_decl list;
	s_pfuncs: s_func list;
}

let rec get_range l (a:char) b =
	let lower = Char.code a in
	let upper = Char.code b in
	if lower = upper then
		a :: l
	else 
		get_range (a :: l) (Char.chr (lower+1)) b 

let get_dt fdc = match fdc with
	| Func_Decl(_, dt, it, _, den) -> (dt, it, den)
	| Var_Decl(_, _, dt, den) -> (dt, [dt], den)

(* returns string of the primitive type *)

let string_of_prim_type = function
  | Int -> "int"
  | String -> "string"
  | Note -> "note"
  | Measurepoo -> "measure"
  | Phrase -> "phrase"
  | Song -> "song"
  | TimeSig -> "timesig"
  | Instr -> "instr"
  | Tempo -> "tempo"
  | Intlist -> "int_list"
  | Stringlist -> "str_list"
  | Null_Type -> "null"


(* returns type of expr *)

let rec type_of_expr here = match here with
	S_Int_Lit(_,t) -> t
  | S_String_Lit(_,t) -> t
  | S_Id(_,t) -> t
  | S_Note(_,_,t) -> t
  | S_TimeSig(_,_,t) -> t
  | S_Instr(_,t) -> t
  | S_Tempo(_,t) -> t
  | S_Measure(_, _, t) -> t
  | S_Phrase(_, _, t) -> t
  | S_Song( _, _, t) -> t
  | S_Binop(_,_,_,t) -> t 
  | S_Arr (_, t) -> let tpe = (match t with 
  		Int -> Intlist
  		| String -> Stringlist
  		| Note -> Measurepoo
  		| Measurepoo -> Phrase
  		| Phrase -> Song) in tpe
  | S_Call (_, _, _, _, t) -> t
  | S_Index (_, _, t) -> let tpe = (match t with
        Intlist -> Int
        | Stringlist -> String
        | Measurepoo -> let hack = S_Note(5, 'a', Note) in
            let bs = (match hack with S_Note(i, d, k) -> k) in
                bs
        | Phrase -> Measurepoo
        | Song -> Phrase) in tpe
  | S_Db_Arr(_, ar) -> let b = type_of_expr ar in b
  | S_Noexpr -> Null_Type 
  | _ -> raise(Failure("Error: Could not match type in type_of_expr."))


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

let confirm_var var env = 
	let decl = Table.get_decl (fst_of_three var) env in
	match decl with
		Func_Decl(f) -> raise(Failure("Error: symbol is not a variable"))
	  | Var_Decl(v) -> 	let (vname, varray, vtype, id) = v in
			(vname, varray, vtype, id)

let confirm_func_decl name env =
	let decl = Table.get_decl name env in
	match decl with 
		Func_Decl(f) -> name
		| _ -> raise(Failure("Error: id " ^ name ^ " not a function"))

let confirm_id_get_type id env = 
	let decl = Table.get_decl id env in
	match decl with
		Var_Decl(v) -> let (_, _, t, _) = v in t
		| _ -> raise(Failure("Error: id " ^ id ^ " not a variable.")) 

(* get variables *)

let get_vars li =
	(match li with 
		VarDecl(v) -> 	
			(match v with
				Assign(dt, iden, v) -> 
					(match dt with 
						Int -> (iden, false, dt) 
						| Note -> (iden, false, dt)
						| Measurepoo -> (iden, false, dt)
						| String -> (iden, false, dt)
                        | TimeSig -> (iden, false, dt) 
                        | Instr -> (iden, false, dt)
                        | Tempo -> (iden, false, dt)
                        | _ -> (iden, true, dt))
				| Update(iden, v) -> ("", false, Wild)
				| Index_Update(expr_1, expr_2) -> ("", false, Wild))
		| _ ->	("", false, Wild)) 

(* confirm correct format of a binary operation *)

let confirm_binop l r op =
	let tl = type_of_expr l in
	let tr = type_of_expr r in
	match op with 
		Plus | Minus | Times | Divide  -> (match (tl, tr) with
			Int, Int -> Int
			| Note, Int -> Note	
			| _, _ -> raise(Failure("Error: Cannot apply + - * / op to types " ^ string_of_prim_type tl ^ " + " ^ string_of_prim_type tr)))
		| Equal | Neq -> if tl = tr then Int else (match(tl, tr) with
			_, _ -> raise(Failure("Error: Cannot apply == !=  op to types " ^ string_of_prim_type tl ^ " + " ^ string_of_prim_type tr)))
		| Less | Greater | Leq | Geq-> (match (tl, tr) with
			Int, Int -> Int
			| Note, Int -> Int	
			| Note, Note -> Int
			| _, _ -> raise(Failure("Error: Cannot apply < > <= >=  op to types " ^ string_of_prim_type tl ^ " + " ^ string_of_prim_type tr)))
		| And | Or -> (match (tl, tr) with
			Int, Int-> Int
			| _, _ -> raise(Failure("Error: Cannot apply && ||  op to types " ^ string_of_prim_type tl ^ " + " ^ string_of_prim_type tr)))

(* map function to list *)

let rec map1 lst func env boo = 
	match lst with 
		[] -> []
		| head :: tail -> 
			let ret = func head env boo in 
				ret :: map1 tail func env boo

(* map function to 2d list *)

let rec map2 lst func env boo =
	match lst with
		[] -> []
		| head :: tail ->
			let ret = map1 head func env boo in
				ret :: map2 tail func env boo

(* map function to 3d list *)

let rec map3 lst func env boo = 
	match lst with 
		[] -> []
		| head :: tail ->
			let ret = map2 head func env boo in
				ret :: map3 tail func env boo

(* convert AST expressions into SAST expressions *)

let rec confirm_expr ex env boo =
	match ex with
	IntLit(i) 			-> S_Int_Lit(i, Int)
	| Id(st)			-> S_Id(st, confirm_id_get_type st env)
	| String_Lit(st)	-> S_String_Lit(st, String)
	| Note(ct, nt)		-> S_Note(ct, nt, Note)
	| Measure(nt_list, time) -> let new_time = confirm_expr time env true in
								let s_note_list = map1 nt_list confirm_expr env true in
								S_Measure(s_note_list, new_time, Measurepoo)
	| Phrase(m_l, inst) -> let verified_list = map1 m_l confirm_expr env boo in 
						   S_Phrase( verified_list, confirm_expr inst env boo, Phrase) 
	| Song(s_l, tempo) -> S_Song(map1 s_l confirm_expr env boo, confirm_expr tempo env boo, Song) 
    | TimeSig(num, den) -> S_TimeSig(num, den, TimeSig)
    | Instr(st)         -> S_Instr(st, Instr)
    | Tempo(i)          -> S_Tempo(i, Tempo)
    | Index(str, i)     ->
            let st = get_id_type str env in
            let rl_int = (match i with IntLit(v) -> S_Int_Lit(v, Int)
        			| Id(nme) -> S_Id(nme, Int)) in
            S_Index(str, rl_int, st)
	| Binop(lft, op, rgt) ->
		let l = confirm_expr lft env false in
		let r = confirm_expr rgt env false in
		let tp = confirm_binop l r op in
		let lt = type_of_expr l in
		let rt = type_of_expr r in
		if lt = rt then S_Binop(l, op, r, tp)
		else (match (lt,rt) with
		Note, Int -> S_Binop(l, op, r, Note)
		| _ -> raise(Failure("Error: Illegal operation on illegal pair of types " ^ string_of_prim_type lt ^ " and " ^ string_of_prim_type rt)) )
	| BasicList(li) -> 
		let (it, ty) = check_arr li env in
		S_Arr(it, ty)
	| FuncList(li, fl) -> 
		let mapval fu (arg:expr) = (* for array to be created *)
			let (nme, ag) = 
				(match fu with
				FunkCall(i, e) -> (i, e)
				| _ -> raise(Failure("Error: Specified string in FuncList is not a valid function."))) in
			let fn_decl = Table.get_decl nme env in
			let (dt, it, de) = get_dt fn_decl in
			let typ = (match arg with
				IntLit(i) -> Int
				| Note(_, _) -> Note
				| String_Lit(_) -> String
				| Id(st) -> let v_decl = Table.get_decl st env in
					let (t_st, _, _) = get_dt v_decl in
					t_st
				| FunkCall(id, args) -> let f_decl = Table.get_decl id env in
					let (ty_funk, _, _ ) = get_dt f_decl in ty_funk
                | Index(id, place) -> let var_dec = Table.get_decl id env in
                    let (t_obj, _, _) = get_dt var_dec in t_obj
                 | Measure(_, _) -> Measurepoo
                 | Phrase(_, _) -> Phrase
                 | Song(_, _) -> Song
                 | _ -> raise(Failure("A function cannot be called on this type."))
 				) in
			let verify_type_and_vars tok = 
				let nwvar =  check_ex_list tok env in
				let nwtp = check_call_and_type nme nwvar env in
				nwvar in
			let verify_mod_expr tok = confirm_expr tok env false in
			let ags = verify_type_and_vars ag in
			let i_arg = verify_mod_expr arg in
			if List.mem typ it then
			(match dt with 
			Null_Type ->
				i_arg
			| _ ->	S_Call(nme, i_arg, ags, it, dt))
			else  raise(Failure("Error: Illegal function call " ^ nme ^ " on argument ")) in
		let mapcall fu (arg:expr) = (* for void calls to be executed before*)
			let (nme, ag) = 
				(match fu with
				FunkCall(i, e) -> (i, e)
				| _ -> raise(Failure("Error: Specified string in FuncList is not a valid function."))) in
			let fn_decl = Table.get_decl nme env in
			let (dt, it, de) = get_dt fn_decl in
			let typ = (match arg with
				IntLit(i) -> Int
				| Note(_, _) -> Note
				| String_Lit(_) -> String
				| Id(st) -> let v_decl = Table.get_decl st env in
					let (t_st, _, _) = get_dt v_decl in
					t_st
				| Default -> Wild
				| FunkCall(nme, arg_vals) -> Wild
                | Index(id, place) -> let var_dec = Table.get_decl id env in
                    let (t_obj, _, _) = get_dt var_dec in t_obj
                 | Measure(_, _) -> Measurepoo
                 | Phrase(_, _) -> Phrase
                 | Song(_, _) -> Song
                 | _ -> raise(Failure("A function cannot be called on this type."))

				) in
			let verify_type_and_vars tok = 
				let nwvar =  check_ex_list tok env in
				let nwtp = check_call_and_type nme nwvar env in
				nwvar in
			let verify_mod_expr tok = confirm_expr tok env false in
			let ags = verify_type_and_vars ag in
			let i_arg = verify_mod_expr arg in
			if List.mem typ it then
			(match dt with 
			Null_Type ->
				S_Call(nme, i_arg, ags, it, dt)
			| _ -> S_Noexpr)
			else  raise(Failure("Error: Illegal function call " ^ nme ^ " on an argument.")) in
		let l_calls =  List.map2 mapval li fl in
		let r_calls = List.map2 mapcall (List.rev li) fl in
		let (it, ty) = check_arr fl env in
		let ret = (match boo with 
			 true -> S_Db_Arr(S_Call_lst(r_calls), S_Arr(l_calls, ty))
			 | false -> S_Db_Arr(S_Call_lst(r_calls), S_Noexpr)
			) in ret
	| FunkCall(i, lis) -> 
		let arg_var = check_ex_list lis env in
		let rt_typ = check_call_and_type i arg_var env in
		let decl_f = Table.get_decl i env in
		let (implicit_parm_type, explicit_param_types, arg_types) = get_dt decl_f in
		S_Call(i, (confirm_expr Default env false), arg_var, explicit_param_types, rt_typ)
	| Default -> S_Noexpr

and check_arr arr env = 
	match arr with
	[] -> ([], Null_Type) (* Empty *)
	| head :: tail ->
		let verified_head = confirm_expr head env false in
		let head_type = type_of_expr verified_head in
			let rec verify_list_and_type l t e = match l with
				[] -> ([], t)
				| hd :: tl -> 
					let ve = confirm_expr hd e false in
					let te = type_of_expr ve in
					(ve :: (fst (verify_list_and_type tl te e)), t) in
		(verified_head :: (fst (verify_list_and_type tail head_type env)), head_type) 

and check_ex_list (lst: expr list) env =
	match lst with
	[] -> []
	| head :: tail -> confirm_expr head env false :: check_ex_list tail env


(* confirm correct function calls *)

and check_call_and_type name vargs env =
	let decl = Table.get_decl name env in (* function name in symbol table *)
	let fdecl = match decl with
		Func_Decl(f) -> f                     (* check if it is a function *)
		| _ -> raise(Failure ("Error: " ^ name ^ " is not a function.")) in
	if name = "print" then Int (* note returns wrong type *)
	else if name = "write" then Wild (* note returns wrong type *)
	else if name = "play" then Wild (* note returns wrong type *)
	else if name = "evaluate" then Wild
	else 
		let (_,rtype, _, params,_) = fdecl in
		if (List.length params) = (List.length vargs) then
			let arg_types = List.map type_of_expr vargs in
			if params = arg_types then rtype
			else raise(Failure("Error: Argument types in " ^ name ^ " call do not match formal parameters."))
		else raise(Failure("Error: Function " ^ name ^ " takes " ^ string_of_int (List.length params) ^ " arguments, called with " ^ string_of_int (List.length vargs))) 

(* get the type of an id of a variable *)

and get_id_type den env =
	let mark = Table.get_decl den env in
	let var = match mark with
	Var_Decl(sk) -> sk
	| _ -> raise(Failure ("Error: " ^ den ^ " is not a variable.")) in
	let  (_, _, tp, _)  = var in
	tp 

(* convert AST statements into SAST statements *)

let rec confirm_stmt stmt ret_type env =
	(match stmt with
	Return(e) ->
		let verified_expr = confirm_expr e env false in
		S_Return(verified_expr) 
	| Expr(e) -> 
		let verified_expr = confirm_expr e env false in
		S_expr(verified_expr)
	| VarDecl(mo)	->	(match mo with 
			Assign(typ, id, e) -> (* Verify that id is compatible type to e *)
			let ve = confirm_expr e env true in
			let eid_type = type_of_expr ve in
			if typ = eid_type
				then S_Assign(id, ve, typ)
			else raise(Failure("Error: Return type does not match* " ^ string_of_prim_type eid_type ^ " " ^ string_of_prim_type typ ^ "."))
			| Update(st, ex) ->
				let vid_type = get_id_type st env in
				let de = confirm_expr ex env true in
				let de_tp = type_of_expr de in
				if de_tp = vid_type then S_Assign(st, de, de_tp)
				else raise(Failure("Attempting to assign variable name " ^ st ^ " to value of type " ^ string_of_prim_type de_tp  ^ " 
					when " ^ st ^ " is already defined as a variable of type " ^ string_of_prim_type vid_type ^ "."))
			| Index_Update(expr_1, expr_2) -> let type_1 = (match expr_1 with
						Index(str, exp) -> let typ_known = Table.get_decl str env in
							let (plz, typ, den) = get_dt typ_known in plz
							| _ -> raise(Failure("Error in matching index type"))) in
						let iden = (match expr_1 with
						Index(str, exp) -> str) in
						let idx = (match expr_1 with
						Index(str, exp) -> exp) in 
						let v_exp1 = confirm_expr idx env false in
						let v_exp2 = confirm_expr expr_2 env false in
						S_Index_Update(iden, v_exp1, v_exp2, type_1))
	| If(e, b1, b2) ->
		let verified_expr = confirm_expr e env false in
		if (type_of_expr verified_expr) = Int then
			let vb1 = confirm_block b1 ret_type (fst env, b1.block_id) in
			let vb2 = confirm_block b2 ret_type (fst env, b2.block_id) in
			S_If(verified_expr, S_CodeBlock(vb1), S_CodeBlock(vb2))
		else raise(Failure("Error: Condition in IF statement must be a boolean expression."))
	| While(condition, block) ->
		let vc = confirm_expr condition env false in
		let vt = type_of_expr vc in 
		if vt = Int then 
			let vb = confirm_block block ret_type (fst env, block.block_id) in
			S_While(vc, vb)
		else raise(Failure("Error: Condition in WHILE statement must be boolean expression.")) 
	| _ -> raise(Failure("Error: Can't map to statement.")))

(* iterates through a list of statements and confirms them *)

and confirm_stmt_list stmt_list ret_type env = 
	match stmt_list with
		  [] -> []
		| head :: tail -> (confirm_stmt head ret_type env) :: (confirm_stmt_list tail ret_type env)

(* function to confirm a block --> confirms each variable and statement *)

and confirm_block block ret_type env =
	let verified_vars = map_to_list_env confirm_var block.locals (fst env, block.block_id) in
	let verified_stmts = confirm_stmt_list block.statements ret_type env in 
	{ s_locals = verified_vars; s_statements = verified_stmts; s_block_id = block.block_id } 

(* goes through each fun, verifies block, arguments, and finally the declaration *)

let confirm_func func env =
	let verified_block = confirm_block func.body func.ret_type (fst env, func.body.block_id) in
	let verified_args = map_to_list_env confirm_var func.args (fst env, func.body.block_id) in
	let verified_func_decl = confirm_func_decl func.fname env in 
    { s_f_type = func.f_type; s_fname = verified_func_decl; s_ret_type = func.ret_type; s_formals = verified_args; s_fblock = verified_block }

(* SAST begins here - first function called: confirm_semantics *)

let confirm_semantics program env = 
	let main_stmts = traverse_main drop_funk (program.stmts) in 
	let main_vars = traverse_main get_vars main_stmts in 
	let g_var_val = List.filter (fun x -> x <> ("", false, Wild)) main_vars in
	let verified_gvar_list = map_to_list_env confirm_var g_var_val env in  
	let main_func = confirm_func ({fname = "main"; ret_type = Null_Type; f_type = []; args = []; body = {locals = []; statements = List.rev main_stmts; block_id = 0}}) env in 
	let verified_func_list = main_func :: map_to_list_env confirm_func program.funcs env in
	let () = prerr_endline "// Passed semantic checking \n" in
		{ s_pfuncs = List.rev verified_func_list; s_gvars = List.rev verified_gvar_list} 