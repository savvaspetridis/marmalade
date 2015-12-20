(*
* Marmalade Abstract Syntax Tree
*)

type op = Plus | Minus | Times | Divide | Equal | Neq | Less | Leq | Greater | Geq | And | Or

type declare_type = Int | Note | String | Song | Phrase | Measurepoo | TimeSig |
Instr | Tempo | List | Intlist | Stringlist | Wild | Null_Type | Default

type char_pair = Ranges of char * char

type var = string * bool * declare_type

type range = int * int

type expr = 
	IntLit of int
	| Id of string
	| String_Lit of string
	| Note of int * char
    | TimeSig of int * int
    | Instr of string
    | Tempo of int
    | Index of string * expr
    | Default
    | Msk_list of expr * expr
    | Measure of expr list * expr (* list of notes, and its time signature *)
    | Phrase of expr list * expr (* list of measures and an instrument *)
    | Song of expr list * expr (* list of phrases and a BPM *)
	| Regex of special_exp
	| Binop of expr * op * expr
	| BasicList of expr list
	| FuncList of expr list * expr list
    | FunkCall of string * expr list

and special_exp = {ids: string list; bounds: char_pair list list}

type vmod =
	Assign of declare_type * string * expr  (* declare a new variable with its type *)
	| Update of string * expr (* reassign a value to a previously declared variable *)

type stmt = 
	Expr of expr
	| VarDecl of vmod
	| If of expr * block * block
	| While of expr * block
	| Return of expr
	| Fdecl of fdecl
	| Null_Type
	| None

(* each block has a list of variables, statments, and an id *)

and block = {
	locals: var list;
	statements: stmt list;
	block_id: int
}

(* function declaration *)

and fdecl = {
    fname : string;
    ret_type : declare_type;
    f_type : declare_type list;
    args : var list;
    body : block;
}

type scope_var_decl = string * bool * declare_type * int

type scope_func_decl = string * declare_type * declare_type list * declare_type list * int

type decl = 
    Func_Decl of scope_func_decl
  | Var_Decl of scope_var_decl

type program = {stmts: stmt list; funcs: fdecl list}
