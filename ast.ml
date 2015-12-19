(*
* Marmadizzle
*)


type op = Plus | Minus | Times | Divide | Equal | Neq | Less | Leq | Greater | Geq | And | Or
(*
type note_type = 
	S of string
	| E of string
	| Q of string 
	| H of string
	| W of string 
*)
type declare_type = Int | Note | String | Song | Phrase | Measurepoo | TimeSig |
Instr | Tempo | List | Intlist | Stringlist | Wild | Null_Type | Default
(*
type funk_expr = 
    IntLit of int
    | ID of string
    | String_Lit of string
    | Note of int * note_type
    | Binop of funk_expr * op * funk_expr 
    | BasicList of funk_expr list

type invocation = 
    FunkCall of string * funk_expr list
*)

(*
type prim_type = 
	Int
	| Note
	| String
	| Song
	| Phrase
	| Measure
	| List
	| Intlist
	| Stringlist

*)




(*type scope_var_decl = string * declare_type * int*)

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
    (* PARSE THESE *)
    | Measure of expr list * expr (* list of notes, TimeSig *)
    | Phrase of expr list * expr (* list of 'measures' (Notes in them), list of their Time-signatures, 
													and an instrument *)
    | Song of expr list * expr (* list of phrases (Notes in them), 
																	list of each measure in each phrase's
																	 time-signature, list of all instruments for each
																	 phrase, and a BPM *)
	| Regex of special_exp
	| Binop of expr * op * expr
	| BasicList of expr list
	| FuncList of expr list * expr list
    | FunkCall of string * expr list

(*type invocation = 
    FunkCall of string * expr list
*)
and special_exp = {ids: string list; bounds: char_pair list list}


type vmod =
	Assign of declare_type * string * expr 
	| Append of string * expr list
	| Append_Assign of declare_type * string * expr list
	| Update of string * expr


type stmt = 
	Expr of expr
	| VarDecl of vmod
	(*| Fdecl of string * declare_type * expr list * stmt list*)
	| If of expr * block * block
	| While of expr * block
	| Return of expr
	| Fdecl of fdecl
	| Null_Type
	| None

and block = {
	locals: var list;
	statements: stmt list;
	block_id: int
}

and fdecl = {
    fname : string;
    ret_type : declare_type;
    f_type : declare_type list;
    args : var list;
(*  formals : string list;
    locals : string list; *)
    body : block;
}


type scope_var_decl = string * bool * declare_type * int

type scope_func_decl = string * declare_type * declare_type list * declare_type list * int

type decl = 
    Func_Decl of scope_func_decl
  | Var_Decl of scope_var_decl

type program = {stmts: stmt list; funcs: fdecl list}
