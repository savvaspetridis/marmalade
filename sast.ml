(*
* SAST for Marmalade
* much of this was adapted from Corgi
*)

open Ast

let fst_of_three (t, _, _) = t
let snd_of_three (_, t, _) = t
let thrd_of_three (_, _, t) = t



type s_expr = 
	S_Int_Lit of int * declare_type
	| S_Id of string * declare_type
	| S_String_Lit of string * declare_type
	| S_Note of int * char * declare_type
	| S_Measure of expr list * expr * declare_type
	| S_Phrase of expr list list * expr list * expr * declare_type
	| S_Song of expr list list list * expr list list * expr list * expr * declare_type
    | S_TimeSig of int * int * declare_type
    | S_Instr of string * declare_type
    | S_Tempo of int * declare_type
    | S_Binop of s_expr * op * s_expr * declare_type
	| S_Call of string * s_expr * s_expr list * declare_type list * declare_type
	| S_Index of string * int * declare_type
	| S_Arr of s_expr list * declare_type
	| S_Db_Arr of s_expr * s_expr
	| S_Call_lst of s_expr list
	| S_Noexpr 
	(*| S_Measure of s_expr list * s_expr
	| S_Phrase of s_expr list list * s_expr list * s_expr
	| S_Song of s_expr list list list * s_expr list list * s_expr list * s_expr*)

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


let rec type_of_expr here = match here with
	S_Int_Lit(_,t) -> t
  | S_String_Lit(_,t) -> t
  | S_Id(_,t) -> t
  | S_Note(_,_,t) -> t
  | S_TimeSig(_,_,t) -> t
  | S_Instr(_,t) -> t
  | S_Tempo(_,t) -> t
  | S_Binop(_,_,_,t) -> t 
  | S_Arr (_, t) -> let tpe = (match t with 
  		Int -> Intlist
  		| String -> Stringlist
  		| Note -> Measurepoo
  		| Measurepoo -> Phrase
  		| Phrase -> Song) in tpe
  | S_Call (_, _, _, _, t) -> t
  | S_Index (_, _, t) -> t
  | S_Db_Arr(_, ar) -> let b = type_of_expr ar in b
  | S_Noexpr -> Null_Type 
  | _ -> raise(Failure("could not match type in type_of_expr "))


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

let verify_var var env = 
	(*let () = Printf.printf "## in verifying var ## \n" in *) let () = print_string ((fst_of_three var) ^ " Nulled??") in
	let decl = Table.get_decl (fst_of_three var) env in
	match decl with
		Func_Decl(f) -> raise(Failure("symbol is not a variable"))
	  | Var_Decl(v) -> 	let (vname, varray, vtype, id) = v in
			(vname, varray, vtype, id)

let verify_is_func_decl name env =
	let () = print_string (name ^ " Nulled6??") in
	let decl = Table.get_decl name env in
	match decl with 
		Func_Decl(f) -> name
		| _ -> raise(Failure("id " ^ name ^ " not a function"))

let verify_id_get_type id env = 
	let () = print_string (id ^ " Nulled7??") in
	let decl = Table.get_decl id env in
	match decl with
		Var_Decl(v) -> let (_, _, t, _) = v in t
		| _ -> raise(Failure("id " ^ id ^ " not a variable.")) 

(*let rm_one var l = 
	let l = a :: (_ as t) -> if a = var then rm_one t 
		else rm_one a :: rm_one t *)



let get_vars li =
	match li with 
		VarDecl(v) -> 	
			let () = Printf.printf "HEY \n" in
			(match v with
				Assign(dt, iden, v) -> 
					let () = Printf.printf "ASSIGN - SAST \n" in
					(match dt with 
						Int -> (iden, false, dt) 
						| Note -> (iden, false, dt)
						| Measurepoo -> (iden, false, dt)
						| String -> (iden, false, dt)
                        | TimeSig -> (iden, false, dt) 
                        | Instr -> (iden, false, dt)
                        | Tempo -> (iden, false, dt)
                        | _ -> (iden, true, dt))
				| Update(iden, v)	-> let () = Printf.printf "UPDATE - SAST \n" in
					("", false, Wild)
				| Append_Assign(dt, iden, aplist) ->
					let () = Printf.printf "APPEND ASSIGN - SAST \n" in
					(match dt with
						Note -> (iden, false, dt)
						| Measurepoo -> (iden, true, dt)

					))
		| _ ->						("", false, Wild)

let verify_binop l r op =
	let tl = type_of_expr l in
	let tr = type_of_expr r in
	match op with 
		Plus | Minus | Times | Divide  -> (match (tl, tr) with
			Int, Int -> Int
			| Note, Int -> Note	
			| _, _ -> raise(Failure("Cannot apply + - * / op to types " ^ string_of_prim_type tl ^ " + " ^ string_of_prim_type tr)))
		| Equal | Neq -> if tl = tr then Int else (match(tl, tr) with
			_, _ -> raise(Failure("Cannot apply == !=  op to types " ^ string_of_prim_type tl ^ " + " ^ string_of_prim_type tr)))
		| Less | Greater | Leq | Geq-> (match (tl, tr) with
			Int, Int -> Int
			| Note, Int -> Int	
			| Note, Note -> Int
			| _, _ -> raise(Failure("Cannot apply < > <= >=  op to types " ^ string_of_prim_type tl ^ " + " ^ string_of_prim_type tr)))
		| And | Or -> (match (tl, tr) with
			Int, Int-> Int
			| _, _ -> raise(Failure("Cannot apply && ||  op to types " ^ string_of_prim_type tl ^ " + " ^ string_of_prim_type tr)))



let rec verify_expr ex env boo =
	match ex with
	IntLit(i) 			-> S_Int_Lit(i, Int)
	| Id(st)			-> S_Id(st, verify_id_get_type st env)
	| String_Lit(st)	-> S_String_Lit(st, String)
	| Note(ct, nt)		-> S_Note(ct, nt, Note)
    | TimeSig(num, den) -> S_TimeSig(num, den, TimeSig)
    | Instr(st)         -> S_Instr(st, Instr)
    | Tempo(i)          -> S_Tempo(i, Tempo)
	| Binop(lft, op, rgt) ->
		let l = verify_expr lft env false in
		let r = verify_expr rgt env false in
		let tp = verify_binop l r op in
		let lt = type_of_expr l in
		let rt = type_of_expr r in
		if lt = rt then S_Binop(l, op, r, tp)
		else (match (lt,rt) with
		Note, Int -> S_Binop(l, op, r, Note)
		| _ -> raise(Failure("Illigal operation on illigal pair of types " ^ string_of_prim_type lt ^ " and " ^ string_of_prim_type rt)) )
	| BasicList(li) -> 
		let (it, ty) = check_arr li env in
		S_Arr(it, ty)
	| FuncList(li, fl) -> 
		let mapval fu (arg:expr) = (* for array to be created *)
			let (nme, ag) = 
				(match fu with
				FunkCall(i, e) -> (i, e)
				| _ -> raise(Failure("Specified string in FuncList is not a valid function."))) in
			let fn_decl = Table.get_decl nme env in
			let (dt, it, de) = get_dt fn_decl in
			let typ = (match arg with
				IntLit(i) -> Int
				| Note(_, _) -> Note
				| String_Lit(_) -> String
				| Id(st) -> let () = print_string (nme ^ " Nulled4??") in let v_decl = Table.get_decl st env in
					let (t_st, _, _) = get_dt v_decl in
					t_st
				) in
			let verify_type_and_vars tok = 
				let nwvar =  check_ex_list tok env in
				let nwtp = check_call_and_type nme nwvar env in
				nwvar in
			let verify_mod_expr tok = verify_expr tok env false in
			let ags = verify_type_and_vars ag in
			let i_arg = verify_mod_expr arg in
			(*let (lis, tp) = *)
			if List.mem typ it then
			(match dt with 
			Null_Type ->
				i_arg
			| _ ->	S_Call(nme, i_arg, ags, it, dt))
			else  raise(Failure("Illigal function call " ^ nme ^ " on argument ")) in
		let mapcall fu (arg:expr) = (* for void calls to be executed before*)
			let (nme, ag) = 
				(match fu with
				FunkCall(i, e) -> (i, e)
				| _ -> raise(Failure("Specified string in FuncList is not a valid function."))) in
			let fn_decl = Table.get_decl nme env in
			let () = print_string (nme ^ " *Nulled??") in
			let (dt, it, de) = get_dt fn_decl in
			let typ = (match arg with
				IntLit(i) -> Int
				| Note(_, _) -> Note
				| String_Lit(_) -> String
				| Id(st) -> let () = print_string (st ^ " Nulled??") in let v_decl = Table.get_decl st env in
					let (t_st, _, _) = get_dt v_decl in
					t_st
				) in
			let verify_type_and_vars tok = 
				let nwvar =  check_ex_list tok env in
				let nwtp = check_call_and_type nme nwvar env in
				nwvar in
			let verify_mod_expr tok = verify_expr tok env false in
			let ags = verify_type_and_vars ag in
			let i_arg = verify_mod_expr arg in
			(*let (lis, tp) = *)
			if List.mem typ it then
			(match dt with 
			Null_Type ->
				S_Call(nme, i_arg, ags, it, dt)
			| _ -> S_Noexpr)
			else  raise(Failure("Illigal function call " ^ nme ^ " on an argument ")) in
		let l_calls =  List.map2 mapval li fl in
		(*S_Noexpr in
		if boo = true then l_calls = List.map2 mapval li fl else l_calls = S_Noexpr in *)
		let r_calls = List.map2 mapcall li fl in
		let (it, ty) = check_arr fl env in
		let ret = (match boo with 
			 true -> S_Db_Arr(S_Call_lst(r_calls), S_Arr(l_calls, ty))
			 | false -> S_Db_Arr(S_Call_lst(r_calls), S_Noexpr)
			) in ret
		(*S_Db_Arr(S_Call_lst(r_calls), S_Arr(l_calls, ty))*)
	(*| FunkCall(i, lis) -> 
		let arg_var = check_ex_list lis env in
		let rt_typ = check_call_and_type i arg_var env in
		S_Call(i, arg_var, rt_typ)*)

and check_arr arr env = 
	match arr with
	[] -> ([], Null_Type) (* Empty *)
	| head :: tail ->
		let verified_head = verify_expr head env false in
		let head_type = type_of_expr verified_head in
			let rec verify_list_and_type l t e = match l with
				[] -> ([], t)
				| hd :: tl -> 
					let ve = verify_expr hd e false in
					let te = type_of_expr ve in
					if t = te then (ve :: (fst (verify_list_and_type tl te e)), t) 
					else raise (Failure "Elements of inconsistent types in Array")
			in
		(verified_head :: (fst (verify_list_and_type tail head_type env)), head_type) 

and check_ex_list (lst: expr list) env =
	match lst with
	[] -> []
	| head :: tail -> verify_expr head env false :: check_ex_list tail env

and check_call_and_type name vargs env =
	let decl = let () = print_string (name ^ " Nulled3??") in Table.get_decl name env in (* function name in symbol table *)
	let fdecl = match decl with
		Func_Decl(f) -> f                     (* check if it is a function *)
		| _ -> raise(Failure (name ^ " is not a function")) in
	if name = "print" then Int (* note returns wrong type *)
	else if name = "write" then Wild (* note returns wrong type *)
	else if name = "play" then Wild (* note returns wrong type *)
	else 
		let (_,rtype, _, params,_) = fdecl in
		if (List.length params) = (List.length vargs) then
			(*let s_params = check_ex_list vargs in*)
			let arg_types = List.map type_of_expr vargs in
			if params = arg_types then rtype
			else raise(Failure("Argument types in " ^ name ^ " call do not match formal parameters."))
		else raise(Failure("Function " ^ name ^ " takes " ^ string_of_int (List.length params) ^ " arguments, called with " ^ string_of_int (List.length vargs))) 

let get_id_type den env =
	let mark = let () = print_string (den ^ " Nulled2??") in Table.get_decl den env in
	let var = match mark with
	Var_Decl(sk) -> sk
	| _ -> raise(Failure (den ^ " is not a variable")) in
	let  (_, _, tp, _)  = var in
	tp 
(*
	NOTE --> This function was originally written to parse note, phrase and song literals and for that it will work.
				The issue is, this will have a problem with Ids.
				The way we will deal with it is by parsing as follows:

				When finds an id in the list of expressions, we'll append them to a list of ids which is the second value
				in the tuple compress_append returns.

				The goal of compress_append is to take an append_list and end up with a tupe of a list of Note, measure, phrase and song objects.
				and a list of ids.

				Somehow you'll have to figure out if an id or literal came first (possibly by putting some expr on the top of 
				either list?)
				

*)

(* Savvas finish this function and write type_of_expr_ast function, model it on type_of_expr function except with ast objects*)
(*let compress_app_list typ app_list env = *)






let rec compress_append t verified_list append_list env = 
	(match append_list with 
		[] -> verified_list (* empty append list *)
		| [fst; snd] -> let last_two = (fst, snd) in (* match last two elements of append list *)
			(match last_two with
				(Note(n_1, d_1) , Note(n_2, d_2)) ->  							(* ~~~ NOTE, NOTE ~~~ *)
						(match t with 
							Measurepoo ->
								let meas = S_Measure([Note(n_1, d_1); Note(n_2, d_2)], Default, Measurepoo) in
								let updated_list = (verified_list@[meas]) in 
								compress_append t updated_list [] env
							| Phrase ->
								let meas = Measure([Note(n_1, d_1); Note(n_2, d_2)], Default) in 
								let updated_list = (verified_list@[S_Phrase([[meas]], [], Default, Phrase)]) in 
								compress_append t updated_list [] env
							| _ -> raise(Failure("Bad")))
				| (Measure(ns, ts), Note(n_1, d_1)) -> 							(* ~~~ MEASURE, NOTE ~~~ *)
					let new_note = Note(n_1, d_1) in 
						(match t with
							Measurepoo -> 
								let meas = S_Measure(ns@[new_note], ts, Measurepoo) in 
								let updated_list = (verified_list@[meas]) in 
								compress_append t updated_list [] env
							| Phrase ->
								let meas = Measure(ns@[new_note], ts) in 
								let updated_list = (verified_list@[S_Phrase([[meas]], [], Default, Phrase)]) in 
								compress_append t updated_list [] env
							| _ -> raise(Failure("Bad")))
				| (Measure(ns_1, ts_1), Measure(ns_2, ts_2)) -> 				(* ~~~ MEASURE, MEASURE ~~~ *)
					let new_phrase = S_Phrase([ns_1; ns_2], [ts_1; ts_2], Default, Phrase) in 
						(match t with
							Phrase ->
								let updated_list = (verified_list@[new_phrase]) in 
								compress_append t updated_list [] env
							| Song -> 
								let new_song = S_Song([[ns_1; ns_2]], [[ts_1; ts_2]], [], Default, Song) in 
								let updated_list = (verified_list@[new_song]) in
								compress_append t updated_list [] env
							| _ -> raise(Failure("Bad")))) 
				(*| (Phrase(ns_1, ts_1, instrum), Measure(ns_2, ts_2)) ->
					let 

						



						let m_f = Phrase(ns_1 @ [ns_2], ts_1 @ [ts_2], instrum) in
						if t = Phrase || t = Song then ([m_f], []) 
					else raise(Failure("A measure cannot be appended to a " string_of_prim_type t )) *)
				

				)



	(* OLD BELOW *)

	(*

	let rec compress_append lst t =  (* might need to add return list argument *)
		let new_l = match lst with
		([], []) -> ([], [])
		| ([fst; snd], id_list) -> let n_val = (fst,snd) in



				(* you're gonna have to figure out how to parse ids 

					One way to do this. 

					Take fst and add it to return list *) 



				let f_val =(match n_val with
				(Note(n_1, d_1), Note(n_2, d_2)) -> 
						let m_f = Measure([fst; snd], Default) in
						if t = Measure || t = Phrase then ([m_f], [])
					else raise(Failure("A measure cannot be appended to a " string_of_prim_type t )) 
				| (Measure(ns, ts), Note(n_1, d_1)) -> 
						let m_f = Measure(ns @ [n_1], ts) in
						if t = Measure || t = Phrase then ([m_f], [])
					else raise(Failure("A measure cannot be appended to a " string_of_prim_type t )) 
				| (Measure(ns_1, ts_1), Measure(ns_2, ts_2)) ->
						let m_f = Phrase([ns_1; ns_2], [ts_1; ts_2], Default) in
						if t = Phrase || t = Song then ([m_f], []) 
					else raise(Failure("A measure cannot be appended to a " string_of_prim_type t )) 
				| (Phrase(ns_1, ts_1, instrum), Measure(ns_2, ts_2)) -> 
						let m_f = Phrase(ns_1 @ [ns_2], ts_1 @ [ts_2], instrum) in
						if t = Phrase || t = Song then ([m_f], []) 
					else raise(Failure("A measure cannot be appended to a " string_of_prim_type t )) 
				| (Phrase(ns_1, ts_1, instrum_1), Phrase(ns_2, ts_2, instrum_2)) -> 
						let m_f = Song([ns_1; ns_2], [ts_1; ts_2], [instrum_1; instrum_2], Default ) in
						if t = Song then ([m_f], []) 
					else raise(Failure("A measure cannot be appended to a " string_of_prim_type t )) 
				| (Song(ns_1, ts_1, instrum_1, bpm), Phrase(ns_2, ts_2, instrum_2)) -> 
						let m_f = Song(ns_1 :: ns_2, ts_1 :: ts_2, instrum_1 :: instrum_2, bpm)
						if t = Song then ([m_f], []) 
					else raise(Failure("A measure cannot be appended to a " string_of_prim_type t )) 
				| (Song(ns_1, ts_1, instrum_1, bpm_1), Song(ns_2, ts_2, instrum_2, bpm_2)) ->
						let m_f = Song(ns_1 @ ns_2, ts_1 @ ts_2, instrum_1 @ instrum_2, bpm_1)
						if t = Song then ([m_f], [] )
					else raise(Failure("A measure cannot be appended to a " string_of_prim_type t )) 											
					)
						 in f_val
		| fst :: (snd :: (thr :: _ as tl)) -> let n_val = let comp_triple = (fst, snd, thr) in 
			let action = (match (fst, snd, thr) with



				| (Id(string_1), Id(string_2), Id(string_3)) -> let decl = Table.get_decl env in
				| (Song(ns_1, ts_1, instrum_1, bpm_1),Id(string_1), Song(ns_1, ts_1, instrum_1, bpm_1)) ->


				(* here you'll probably have to enumerate all the possiblities with ids and figure out how to put them in the
					list, and then reccurse. Good luck *)




				(Note(n_1, d_1), Note(n_2, d_2), Note(n_3, d_3)) -> (* combine first two notes and reccurse EXAMPLE do others the same way *)
					let fusion = Measure([fst; snd], Default) in
						let new_l = fusion :: (thr :: tl) in
							(compress_append new_l t, [])
				| (Measure(ns, ts), Note(n_2, d_2), Note(n_3, d_3)) -> (* combine measure with note and reccurse *)
				| (Measure(ns_1, ts_1), Measure(ns_2, ts_2), Note(n_3, d_3)) -> (* combine 2nd measure and note *)
				| (Measure(ns_1, ts_1), Measure(ns_2, ts_2), Measure(ns_2, ts_2)) -> (* combine first two measures ... you should get the point *)
				| (Measure(ns_1, ts_1), Note(n_2, d_2), Measure(ns_2, ts_2)) -> 
				| (Phrase(ns_1, ts_1, instrum), Measure(ns_2, ts_2), Note(n_3, d_3)) ->
				| (Phrase(ns_1, ts_1, instrum), Measure(ns_2, ts_2), Measure(ns_2, ts_2)) ->
				| (Phrase(ns_1, ts_1, instrum_1), Phrase(ns_2, ts_2, instrum_2), Measure(ns, ts)) ->
				| (Phrase(ns_1, ts_1, instrum_1), Measure(ns, ts), Phrase(ns_2, ts_2, instrum_2)) ->
				| (Phrase(ns_1, ts_1, instrum_1), Phrase(ns_2, ts_2, instrum_2), Phrase(ns_3, ts_3, instrum_3)) ->
				| (Song(ns_1, ts_1, instrum_1, bpm), Phrase(ns_2, ts_2, instrum_2), Phrase(ns_3, ts_3, instrum_3)) ->
				| (Song(ns_1, ts_1, instrum_1, bpm_1), Song(ns_2, ts_2, instrum_2, bpm_2), Phrase(ns_3, ts_3, instrum_3)) ->
				| (Song(ns_1, ts_1, instrum_1, bpm_1), Phrase(ns_2, ts_2, instrum_2), Song(ns_3, ts_3, instrum_3, bpm_3)) ->
				| (Song(ns_1, ts_1, instrum_1, bpm_1), Song(ns_2, ts_2, instrum_2, bpm_2), Song(ns_3, ts_3, instrum_3, bpm_3)) ->
				| _ -> raise(Failure("Append pattern not among accepted music pairs")) 
			) in compress_append (app_list, [], typ)

*)

let rec verify_stmt stmt ret_type env =
	let () = Printf.printf "in update \n" in
	match stmt with
	Return(e) ->
		let verified_expr = verify_expr e env false in
		if ret_type = type_of_expr verified_expr then S_Return(verified_expr) 
		else raise(Failure "return type does not match**") 
	| Expr(e) -> 
		let verified_expr = verify_expr e env false in
		S_expr(verified_expr)
	| VarDecl(mo)	->	(match mo with 
			Assign(typ, id, e) -> (* Verify that id is compatible type to e *)
			let ve = verify_expr e env true in
			(*let vid_type = get_id_type id env in *)
			let eid_type = type_of_expr ve in
			if typ = eid_type
				then (*let () = Printf.printf "got typ \n" in*) S_Assign(id, ve, typ)
			else raise(Failure("return type does not match* " ^ string_of_prim_type eid_type ^ " " ^ string_of_prim_type typ))
			| Update(st, ex) -> let () = Printf.printf "in update \n" in
				let vid_type = get_id_type st env in
				let de = verify_expr ex env true in
				let de_tp = type_of_expr de in
				if de_tp = vid_type then S_Assign(st, de, de_tp)
				else raise(Failure("Attempting to assign variable name " ^ st ^ " to value of type " ^ string_of_prim_type de_tp  ^ " 
					when " ^ st ^ " is already defined as a variable of type " ^ string_of_prim_type vid_type ^ "."))
			(*| Append(iden, ap_l) -> 
				let typ = get_id_typ iden env in
				let  app_lis = verify_app_list_mod ap_l typ env in
				app_lis*)
			| Append_Assign(ty, stri, ap_l) -> 

				let verified_app_list = compress_append ty [] ap_l env in
				S_Append_Assign(ty, stri, verified_app_list)

				
				(*let app_block = S_Append_block(verified_app_list) in*)



				(*let app_lis = verify_app_list_def ap_l typ env in
				let eval_typ = verify_expr app_lis env true in
				let eid_typ = type_of_expr eval_typ in
				if typ = eid_type
				then (*let () = Printf.printf "got typ \n" in*) S_Assign(stri, app_lis, ty)
			else raise(Failure("return type does not match* " ^ string_of_prim_type eid_type ^ " " ^ string_of_prim_type typ))*)
				)
	| If(e, b1, b2) ->
		let verified_expr = verify_expr e env false in
		if (type_of_expr verified_expr) = Int then
			let vb1 = verify_block b1 ret_type (fst env, b1.block_id) in
			let vb2 = verify_block b2 ret_type (fst env, b2.block_id) in
			S_If(verified_expr, S_CodeBlock(vb1), S_CodeBlock(vb2))
		else raise(Failure("Condition in if statement must be a boolean expression."))
	| While(condition, block) ->
		let vc = verify_expr condition env false in
		let vt = type_of_expr vc in 
		if vt = Int then 
			let vb = verify_block block ret_type (fst env, block.block_id) in
			S_While(vc, vb)
		else raise(Failure("Condition in While statement must be boolean.")) 
	| _ -> raise(Failure("match screw up, can't map to a statement"))

and verify_stmt_list stmt_list ret_type env = 
	match stmt_list with
		  [] -> []
		| head :: tail -> (verify_stmt head ret_type env) :: (verify_stmt_list tail ret_type env)

and verify_block block ret_type env =
	let verified_vars = map_to_list_env verify_var block.locals (fst env, block.block_id) in
	(*let () = Printf.printf "verified vars  \n" in *)
	let verified_stmts = verify_stmt_list block.statements ret_type env in 
	(*let () = Printf.printf "verified stmts \n" in *)
	{ s_locals = verified_vars; s_statements = verified_stmts; s_block_id = block.block_id } 

let verify_func func env =
	let () = Printf.printf "verifying function \n" in 
	let verified_block = verify_block func.body func.ret_type (fst env, func.body.block_id) in
	(*let () = Printf.printf "func.fname" in *)
	let verified_args = map_to_list_env verify_var func.args (fst env, func.body.block_id) in
	let verified_func_decl = verify_is_func_decl func.fname env in 
	{ s_fname = verified_func_decl; s_ret_type = func.ret_type; s_formals = verified_args; s_fblock = verified_block }

let verify_semantics program env = 
	let main_stmts = traverse_main drop_funk ((*List.rev*) program.stmts) in 
	let main_vars = traverse_main get_vars main_stmts in 
	let g_var_val = List.filter (fun x -> x <> ("", false, Wild)) main_vars in
	let () = Printf.printf "got vars \n" in
	let verified_gvar_list = map_to_list_env verify_var g_var_val env in 
	let () = Printf.printf "got global variables \n" in 
	let main_func = verify_func ({fname = "main"; ret_type = Null_Type; f_type = []; args = []; body = {locals = (*verified_gvar_list*) (*List.rev main_vars*) []; statements = List.rev main_stmts; block_id = 0}}) env in 
	(*let () = Printf.printf "created main \n" in *)
	(*let print = verify_func ({fname = "print"; ret_type = Null_Type; f_type = Null_Type; args = []; body = {locals = (*verified_gvar_list*) []; statements = []; block_id = 0}}) env in 
	let play = verify_func ({fname = "play"; ret_type = Null_Type; f_type = Null_Type; args = []; body = {locals = (*verified_gvar_list*) []; statements = []; block_id = 0}}) env in 
	let write = verify_func ({fname = "main"; ret_type = Null_Type; f_type = Null_Type; args = []; body = {locals = (*verified_gvar_list*) []; statements = []; block_id = 0}}) env in *)
	let verified_func_list = (*write :: play :: print ::*)  main_func :: map_to_list_env verify_func program.funcs env in
	let () = prerr_endline "// Passed semantic checking \n" in
		{ s_pfuncs = List.rev verified_func_list; s_gvars = List.rev verified_gvar_list} 