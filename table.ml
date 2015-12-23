(* 
 * table.ml of marmalade
 * Creates table for checking SAST
 *)

open Ast

module StrMap = Map.Make(String)

let table_env (table,_) = table
let scope_env (_,scope) = scope
let type_of_funct_args (_,_,p_type) = p_type

let over_scope = Array.make 1000 0

let rec map func lst env =
	match lst with
		  [] -> env
		| head :: tail ->
			let new_env = func head env in 
				map func tail new_env


let name_scope_str (name:string) env =
	name ^ "_" ^ (string_of_int (scope_env env))

let rec get_scope name env =
    if StrMap.mem (name_scope_str name env) (fst env) then (snd env)
    else if (snd env) = 0 then raise(Failure("Error: Symbol " ^ name ^ " not declared. " ^ string_of_int (snd env)))
    else get_scope name (fst env, over_scope.(snd env))

let rec get_decl name env =
	let key = name_scope_str name env in 
	if StrMap.mem key (fst env) then StrMap.find key (fst env)
	else
		if (snd env) = 0 then raise (Failure("Error: Symbol " ^ name ^ " not declared in current scope" ^ string_of_int (snd env) ^ "."))
    	else get_decl name ((fst env), over_scope.(snd env))

let insert_symb (name:string) (decl:decl) env =
	let key = name_scope_str name env in
    if StrMap.mem key (table_env env)
    then raise(Failure("Error: Symbol " ^ name ^ " declared twice in same scope."))
    else ((StrMap.add key decl (table_env env)), (scope_env env))

let insert_var var env =
	let (name, p_type) = var in
	let is_implicit_array = 
		(match p_type with
		  (Int | Note | String | TimeSig | Instr | Tempo) -> false
		  | _ -> true) in insert_symb name (Var_Decl(name, is_implicit_array, p_type, (scope_env env))) env

let insert_astvar var env =
	let (name, arr_b, typ) = var in
	insert_var (name, typ) env

(* insert stmt - matches first, then inserts *)

let rec insert_stmt stmt env =  
	(match stmt with
	Expr(exp) -> env
	| If(e, bl_1, bl_2) -> let env_1 = insert_code_block bl_1 Wild env in insert_code_block bl_2 Wild env_1
	| While(e, bl) -> insert_code_block bl Wild env
	| Fdecl(fdec) -> insert_funk fdec env
	| VarDecl(chan) -> (match chan with 
		Assign(typ, id, blah) -> insert_var (id, typ) env
	 	| Update(str, exr) ->  env
		| Index_Update(_, _) -> env)
	| _ ->	env )

(* insert contents of a block of code *)

and insert_code_block block return_tp env =  
	let (table, scope) = env in 
	let id = block.block_id in
	let env = map insert_astvar block.locals (table, id) in
	let env = map insert_stmt block.statements env in
	over_scope.(id) <- scope; 
	((table_env env), scope)

(* insert contents of a function into the table *)

and insert_funk func env =
	let (table, scope) = env in
	let arg_names = List.map type_of_funct_args func.args in
	let env = insert_symb func.fname (Func_Decl(func.fname, func.ret_type, func.f_type, arg_names, scope)) env in
	insert_code_block func.body (func.ret_type) ((table_env env), scope)

(* initialize start_env *)

let start_env = 
	let table = StrMap.add "print_0" (Func_Decl("print", Null_Type, [Int; Note;
    String; Song; Phrase; Measurepoo; TimeSig; Instr; Tempo; List ; Intlist ; Stringlist; Wild], [], 0)) StrMap.empty in
    let table = StrMap.add "evaluate_note_0" (Func_Decl("evaluate_note", Note, [Int; Note;
    String; Song; Phrase; Measurepoo; TimeSig; Instr; Tempo; List ; Intlist ; Stringlist; Wild], [Note], 0)) table in
     let table = StrMap.add "evaluate_measure_0" (Func_Decl("evaluate_measure", Measurepoo, [Int; Note;
    String; Song; Phrase; Measurepoo; TimeSig; Instr; Tempo; List ; Intlist ; Stringlist; Wild], [Measurepoo], 0)) table in
    let table = StrMap.add "evaluate_phrase_0" (Func_Decl("evaluate_phrase", Phrase, [Int; Note;
    String; Song; Phrase; Measurepoo; TimeSig; Instr; Tempo; List ; Intlist ; Stringlist; Wild], [Phrase], 0)) table in
    let table = StrMap.add "evaluate_song_0" (Func_Decl("evaluate_song", Song, [Int; Note;
    String; Song; Phrase; Measurepoo; TimeSig; Instr; Tempo; List ; Intlist ; Stringlist; Wild], [Song], 0)) table in
    let table = StrMap.add "length_note_0" (Func_Decl("length_mnote", Int, [Measurepoo; Note; Phrase; Song; Intlist; Stringlist], [Note], 0)) table in
    let table = StrMap.add "length_measure_0" (Func_Decl("length_measure", Int, [Measurepoo; Note; Phrase; Song; Intlist; Stringlist], [Measurepoo], 0)) table in
    let table = StrMap.add "length_phrase_0" (Func_Decl("length_phrase", Int, [Measurepoo; Note; Phrase; Song; Intlist; Stringlist], [Phrase], 0)) table in
    let table = StrMap.add "length_song_0" (Func_Decl("length_measure", Int, [Measurepoo; Note; Phrase; Song; Intlist; Stringlist], [Song], 0)) table in
    let table = StrMap.add "length_int_list_0" (Func_Decl("length_int_list", Int, [Measurepoo; Note; Phrase; Song; Intlist; Stringlist], [Intlist], 0)) table in
    let table = StrMap.add "length_string_list_0" (Func_Decl("length_string_list", Int, [Measurepoo; Note; Phrase; Song; Intlist; Stringlist], [Stringlist], 0)) table in
	let table = StrMap.add "play_0"  (Func_Decl("play", Null_Type, [Note; String; Song; Phrase; Measurepoo; Wild], [], 0)) table in
	let table = StrMap.add "write_0" (Func_Decl("write", Null_Type, [Note; String; Song; Phrase; Measurepoo], [], 0)) table in
	let table = StrMap.add "main_0" (Func_Decl("main", Null_Type, [], [], 0)) table in
	(table, 0)

(* main function in this file -- initiates table, inserts statements and funks *)

let create_table p = 
	let env = start_env in
	let env =  map insert_stmt (List.rev p.stmts) env in
	let env = map insert_funk (List.rev p.funcs) env in 
	let () = Printf.printf "// Symbol Table Created" in 
	env