(*
* Marmadizzle
*)


type op = Plus | Minus | Times | Divide | Equal | Neq | Less | Leq | Greater | Geq
(*
type note_type = 
	S of string
	| E of string
	| Q of string 
	| H of string
	| W of string 
*)
type declare_type = Int | Note | String | Song | Phrase | Measure | List | Intlist | Stringlist | Wild
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
type expr = 
	IntLit of int
	| Id of string
	| String_Lit of string
	| Note of int * char
(*	| Note_E of int * note_type
	| Note_Q of int * note_type
	| Note_H of int * note_type
	| Note_W of int * note_type *)
	| Binop of expr * op * expr
	| BasicList of expr list
	| FuncList of expr list * expr list
    | FunkCall of string * expr list

(*type invocation = 
    FunkCall of string * expr list
*)
type vmod =
	Assign of declare_type * string * expr 
	| Update of string * expr

type stmt = 
	Expr of expr
	| VarDecl of vmod
	(*| Fdecl of fdecl*)
	| Fdecl of string * declare_type * expr list * stmt list

type fdecl = {
    fname : string;
    ret_type : declare_type;
    args : expr list;
(*    formals : string list;
    locals : string list; *)
    body : stmt list;
}

type program = {stmts: stmt list; funcs: fdecl list}
