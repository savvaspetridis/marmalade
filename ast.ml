(*
* Marmadizzle
*
*
*)

type program = stmt list * fdecl list

type op = Plus | Minus | Times | Divide | Equal | Neq | Less | Leq | Greater | Geq

type expr = 
	IntLit of int
	| Id of string
	| Binop of expr * op * expr
	| 

type stmt = 
	Expr of expr
	| Assign of expr * expr



