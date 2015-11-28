(*
* SAST for Marmalade
*)

open Ast



type d_expr = 
	S_IntLit of int * prim_
	| S_Id of string
	| S_String_Lit of string
	| S_Note of int * char
	| S_Binop of expr * op * expr
(* 	need to replace
	| BasicList of expr list
	| FuncList of expr list * expr list
    | FunkCall of string * expr list
*)
	| S_Call
	| 


(*type invocation = 
    FunkCall of string * expr list
*)
type vmod =
	Assign of declare_type * string * expr 
	| Update of string * expr

type d_stmt = 
	Expr of expr
	| VarDecl of vmod
	| D_While of d_expr * d_block
	| D_If of d_expr * d_stmt * d_stmt

and d_block = {
	d_locals : scope_var_decl list;
	d_statements: d_stmt list;
	d_block_id: int;
}

type fdecl = {
    fname : string;
	sast_ret_type : scope_var_decl;
    locals : string list; *)
    body : stmt list;
}

type sast_program = {
	stmts: stmt list; 
	funcs: fdecl list;
}