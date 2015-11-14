(*
* Marmadizzle
*)


type op = Plus | Minus | Times | Divide | Equal | Neq | Less | Leq | Greater | Geq

type note_type = 
	S of string
	| E of string
	| Q of string 
	| H of string
	| W of string 

type declare_type = Int | Note | String | Song | Phrase | Measure | List

type funk_expr = 
    IntLit of int
    | ID of string
    | String_Lit of string
    | Note of int * note_type
    | Binop of funk_expr * op * funk_expr 
    | BasicList of funk_expr list

type invocation = 
    FunkCall of string * funk_expr list

type expr = 
	IntLit of int
	| Id of string
	| String_Lit of string
	| Note of int * note_type
(*	| Note_E of int * note_type
	| Note_Q of int * note_type
	| Note_H of int * note_type
	| Note_W of int * note_type *)
	| Binop of expr * op * expr
	| BasicList of expr list
	| FuncList of invocation list * expr list

(*type invocation = 
    FunkCall of string * expr list
*)
type vmod =
	Assign of declare_type * string * expr 

type stmt = 
	Expr of expr
	| VarDecl of vmod

type fdecl = {
    fname : string;
(*    formals : string list;
    locals : string list; *)
    body : stmt list;
}

type program = stmt list * fdecl list
