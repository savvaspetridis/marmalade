(* 
	Creates table for checking SAST

	Pretty much entirely taken from Corgi

 *)

open Ast

module StrMap = Map.Make(String)

let env_table (table,_) = table
let env_scope (_,scope) = scope
let type_of_funct_args (_,_,p_type) = p_type

let parent_scope = Array.make 1000 0

let rec map_to_list_env func lst env =
	match lst with
		  [] -> env
		| head :: tail ->
			let new_env = func head env in 
				map_to_list_env func tail new_env

(*   need to rewrite 
let string_of_table env =
    let symlist = StrMap.fold
        (fun s t prefix -> (string_of_decl t) :: prefix) (fst env) [] in
    let sorted = List.sort Pervasives.compare symlist in
    String.concat "\n" sorted

 ______________________________________________ *)

let name_scope_str (name:string) env =
	name ^ "_" ^ (string_of_int (env_scope env))

let rec get_scope name env =
    if StrMap.mem (name_scope_str name env) (fst env) then (snd env)
    else if (snd env) = 0 then raise(Failure("symbol " ^ name ^ " not declared. " ^ string_of_int (snd env)))
    else get_scope name (fst env, parent_scope.(snd env))

let rec get_decl name env =
	let key = name_scope_str name env in 
	if StrMap.mem key (fst env) then StrMap.find key (fst env)
	else
		if (snd env) = 0 then raise (Failure("symbol " ^ name ^ " not declared in current scope" ^ string_of_int (snd env)))
    	else get_decl name ((fst env), parent_scope.(snd env))

let add_symbol (name:string) (decl:decl) env =
	let () = Printf.printf "adding symbol \n" in
	let key = name_scope_str name env in
    if StrMap.mem key (env_table env)
    then raise(Failure("symbol " ^ name ^ " declared twice in same scope"))
    else let () = Printf.printf "in symbol \n" in 
    ((StrMap.add key decl (env_table env)), (env_scope env))


let add_var var exp env =
	let (name, p_type) = var in
	let is_implicit_array = 
		(match p_type with
		  (Int | Note | String | TimeSig | Instr | Tempo) -> false
		  | _ -> true) in let () = Printf.printf "trying to add a var \n" in 
	add_symbol name (Var_Decl(name, is_implicit_array, p_type, (env_scope env))) env

let rec add_stmt stmt env = 
	let () = Printf.printf "adding stmt \n" in 
	match stmt with
	Expr(exp) -> 	let () = Printf.printf "in expr \n" in  env(*(match exp with 
	  Block(block) -> add_block block env
	  | If(expr, block1, block2) -> 
	  		let env = add_block block1 env in add_block block2 env
	  (*| For(expr1, expr2, expr3, block) -> add_block block env*)
	  | While(expr, block) -> add_block block env
	  | '_' -> env )*)
	| If(e, bl_1, bl_2) -> let () = Printf.printf "in expr \n" in let env_1 = add_block bl_1 env in add_block bl_2 env_1
	| While(e, bl) -> let () = Printf.printf "in expr \n" in add_block bl env
	| Fdecl(fdec) -> add_func fdec env
	| VarDecl(chan) -> let () = Printf.printf "in vdec \n" in (match chan with 
		Assign(typ, id, blah) -> let () = Printf.printf "adding assignment \n" in
		add_var (id, typ) blah env
	 	| Update(str, exr) -> env (*let () = Printf.printf "printing update \n" in let dec = get_decl str env in 
	 	let () = Printf.printf "printing update \n" in
	 		(match dec with 
	 			(*Var_Decl(nm, ar, t, _) -> add_var (nm, t) ar env
	 			|*) _ -> raise(Failure("A function cannot be redefined as a variable"))
	 	| _ -> 	let () = Printf.printf "in expr \n" in env ))*))
	| _ ->	env

and add_block block env =  
	let (table, scope) = env in 
	let id = block.block_id in
	let () = Printf.printf "adding block" in
	(*let env = map_to_list_env add_var block.locals (table, id) in*)
	let env = map_to_list_env add_stmt block.statements env in
	parent_scope.(id) <- scope; (* insert scope value into idth element of parent array *)
	((env_table env), scope)

and add_func func env =
	let (table, scope) = env in
	let arg_names = List.map type_of_funct_args func.args in
	let () = Printf.printf "trying to add function\n" in 
	let env = add_symbol func.fname (Func_Decl(func.fname, func.ret_type, func.f_type, arg_names, scope)) env in
	(*let env = map_to_list_env add_var func.formals ((env_table env), func.fblock.block_id) in*)
	add_block func.body ((env_table env), scope)


let base_env = 
	let table = StrMap.add "print_0" (Func_Decl("print", Null_Type, [Int; Note;
    String; Song; Phrase; Measure; TimeSig; Instr; Tempo; List ; Intlist ; Stringlist], [], 0)) StrMap.empty in
	let table = StrMap.add "play_0"  (Func_Decl("play", Null_Type, [Note; String; Song; Phrase; Measure], [], 0)) table in
	let table = StrMap.add "write_0" (Func_Decl("write", Null_Type, [Note; String; Song; Phrase; Measure], [], 0)) table in
	let table = StrMap.add "main_0" (Func_Decl("main", Null_Type, [], [], 0)) table in
	(table, 0)

let build_table p = 
	(*let (vars, funcs) = p in*)
	let env = base_env in
	let env =  map_to_list_env add_stmt (List.rev p.stmts) env in
	let env = map_to_list_env add_func (List.rev p.funcs) env in 
	let () = Printf.printf "through table \n" in 
	env

