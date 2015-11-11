(*
* Marmadizzle
*
*
*)

type program = var_decl 

type op = Plus | Minus | Times | Divide | Equal | Neq | Less | Leq | Greater | Geq

type expr = 
	Lit_Int of int
	| Id of string
	| Binop of expr * op * expr
	| 


