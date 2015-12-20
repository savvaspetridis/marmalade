(* 
 *	Creates table for checking SAST
 *	much of this was adapted from Corgi
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
    else if (snd env) = 0 then raise(Failure("Error: Symbol " ^ name ^ " not declared. " ^ string_of_int (snd env)))
    else get_scope name (fst env, parent_scope.(snd env))

let rec get_decl name env =
	let key = name_scope_str name env in 
	if StrMap.mem key (fst env) then StrMap.find key (fst env)
	else
		if (snd env) = 0 then raise (Failure("Error: Symbol " ^ name ^ " not declared in current scope" ^ string_of_int (snd env) ^ "."))
    	else get_decl name ((fst env), parent_scope.(snd env))

let add_symbol (name:string) (decl:decl) env =
	let () = Printf.printf "adding symbol \n" in
	let key = name_scope_str name env in
    if StrMap.mem key (env_table env)
    then raise(Failure("Error: Symbol " ^ name ^ " declared twice in same scope."))
    else ((StrMap.add key decl (env_table env)), (env_scope env))

let add_var var env =
	let (name, p_type) = var in
	let is_implicit_array = 
		(match p_type with
		  (Int | Note | String | TimeSig | Instr | Tempo) -> false
		  | _ -> true) in add_symbol name (Var_Decl(name, is_implicit_array, p_type, (env_scope env))) env

let add_ast_var var env =
	let (name, arr_b, typ) = var in
	add_var (name, typ) env

let rec add_stmt stmt env =  
	(match stmt with
	Expr(exp) -> env
	| If(e, bl_1, bl_2) -> let env_1 = add_block bl_1 Wild env in add_block bl_2 Wild env_1
	| While(e, bl) -> add_block bl Wild env
	| Fdecl(fdec) -> add_func fdec env
	| VarDecl(chan) -> (match chan with 
		Assign(typ, id, blah) -> add_var (id, typ) env
	 	| Update(str, exr) -> env)
	| _ ->	env)

and add_block block return_tp env =  
	let (table, scope) = env in 
	let id = block.block_id in
	let env = map_to_list_env add_ast_var block.locals (table, id) in
	let env = map_to_list_env add_stmt block.statements env in
	parent_scope.(id) <- scope; 
	((env_table env), scope)

and add_func func env =
	let (table, scope) = env in
	let arg_names = List.map type_of_funct_args func.args in
	let env = add_symbol func.fname (Func_Decl(func.fname, func.ret_type, func.f_type, arg_names, scope)) env in
	add_block func.body (func.ret_type) ((env_table env), scope)

let base_env = 
	let table = StrMap.add "print_0" (Func_Decl("print", Null_Type, [Int; Note;
    String; Song; Phrase; Measurepoo; TimeSig; Instr; Tempo; List ; Intlist ; Stringlist; Wild], [], 0)) StrMap.empty in
    let table = StrMap.add "evaluate_0" (Func_Decl("evaluate", Int, [Int; Note;
    String; Song; Phrase; Measurepoo; TimeSig; Instr; Tempo; List ; Intlist ; Stringlist; Wild], [], 0)) table in
	let table = StrMap.add "play_0"  (Func_Decl("play", Null_Type, [Note; String; Song; Phrase; Measurepoo], [], 0)) table in
	let table = StrMap.add "write_0" (Func_Decl("write", Null_Type, [Note; String; Song; Phrase; Measurepoo], [], 0)) table in
	let table = StrMap.add "main_0" (Func_Decl("main", Null_Type, [], [], 0)) table in
	(table, 0)

let build_table p = 
	let env = base_env in
	let env =  map_to_list_env add_stmt (List.rev p.stmts) env in
	let env = map_to_list_env add_func (List.rev p.funcs) env in 
	let () = Printf.printf "-----------through table------------ \n" in 
	env

