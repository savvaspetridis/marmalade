(*
* Marmadizzle
*
*
*)

type program = stmt list * fdecl list

type op = Plus | Minus | Times | Divide | Equal | Neq | Less | Leq | Greater | Geq
type note_type = S | E |Q | H | W
type declare_type = Int | Note | String | Song | Phrase | Measure

type invocation =
	FunkCall of string * expr list
	

type expr = 
	IntLit of int
	| Id of string
	| String_Lit of string
	| Note_S of int * note_type
	| Note_E of int * note_type
	| Note_Q of int * note_type
	| Note_H of int * note_type
	| Note_W of int * note_type
	| Binop of expr * op * expr
	| BasicList of expr list
	| FuncList of invocation list * expr list

type vmod =
	Assign of declare_type * string * expr 

type stmt = 
	Expr of expr
	| VarDecl of vmod




