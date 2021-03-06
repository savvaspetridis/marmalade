 %{ open Ast 

let scope_id = ref 1

let inc_block_id (u:unit) =
    let x = scope_id.contents in
    scope_id := x + 1; x
%}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token SEMI COMMA PLUS MINUS TIMES
%token INT NOTE STRING MEASURE PHRASE SONG LIST INTLIST STRL
%token DIVIDE ASSIGN EQ NEQ LT LEQ
%token GT GEQ DASH APPEND NOT
%token <char> NOTE_TYPE
%token IF ELSE ELIF AND OR
%token PERIOD COLON
%token RETURN WHILE
%token FUNK AT DOLLAR
%token <int> INT_LIT
%token <string> STRING_LIT ID INSTRUMENT
%token EOF

%nonassoc ELSE
%nonassoc NOELSE
/*%right ASSIGN APPEND
%left LBRACK*/
%left OR
%left AND
/*%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE 
%right NOT*/

%start program
%type <Ast.program> program

%%
program:
    /* nothing */  {{stmts = []; funcs = []} }
    /* List is built backwards */
|	program stmt {{stmts = $2 :: $1.stmts; funcs = $1.funcs}} /* statement head list which is program */

stmt:
	expr SEMI { Expr($1) }
|	vmod SEMI	  { VarDecl($1) }
|	conditional_stmt { $1 }

type_dec:
	INT 	{Int}
|	NOTE 	{Note}
|	MEASURE	{Measure}
|	PHRASE	{Phrase}
|	SONG	{Song}
|	STRING 	{String}
| 	LIST 	{List}
|	INTLIST	{Intlist}
|	STRL 	{Stringlist}

/*
conditional_stmt:
	IF LPAREN expr RPAREN block %prec NOELSE { If($3, $5, [], {locals = []; statements = []; block_id = inc_block_id ()}) }
| 	IF LPAREN expr RPAREN block elif_list ELSE block { If($3, $5, $6, $8) }
|	WHILE LPAREN expr RPAREN block { While($3, $5) }

*/

conditional_stmt:
	IF LPAREN expr RPAREN block %prec NOELSE { If($3, $5, {locals = []; statements = []; block_id = inc_block_id ()}) }
| 	IF LPAREN expr RPAREN block ELSE block { If($3, $5, $7) }
|	WHILE LPAREN expr RPAREN block { While($3, $5) }

block:
	LBRACE stmt_list RBRACE { {locals = []; statements = List.rev $2; block_id = inc_block_id ()} }

/*
elif_list: 
	 nothing  { [] }
	| elif_list elif_stmt { $2 :: $1 } 

elif_stmt:
	ELIF LPAREN expr RPAREN block*/

stmt_list:
    /* nothing */  { [] }
    | stmt_list stmt { $2 :: $1 } 
	
vmod:
	type_dec ID ASSIGN expr {Assign($1, $2, $4)}
|	ID ASSIGN expr {Update($1, $3)}

expr:
  /*| literal							{ $1 }*/
  | app_gen  {$1}
  |	arithmetic {$1}
  /*| ID 								{ Id($1) } 
  | bool_expr {$1}
  | expr PLUS expr                  { Binop($1, Add, $3) }
  | expr MINUS expr                 { Binop($1, Sub, $3) }
  | expr TIMES expr                 { Binop($1, Mult, $3) }
  | expr DIVIDE expr                { Binop($1, Div, $3) }
  | LPAREN expr RPAREN { $2 } */



/*
bool_expr_OR:
	bool_expr_AND {$1}
	| bool_expr_OR OR bool_expr_OR { Binop($1, Or, $3) }

bool_expr_AND:*/





bool_expr: 
	expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }



/* ATTEMPT 1  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

arith: 
	logical_OR_expr { $1 }

primary_expr:
    ID              { Id($1) }
|	literal 		{ $1 }
|	LPAREN expr RPAREN { $2 }

literal:
	INT_LIT {IntLit($1)}
|	note {$1}
|   STRING_LIT {String_Lit($1)}  

multi_expr:
	primary_expr		{ $1 }
|	multi_expr TIMES lit { Binop($1, Mult,$3) }
|   multi_expr DIVIDE lit { Binop($1, Div, $3) } 

add_expr:
	multi_expr { $1 }
|	lit PLUS multi_expr  { Binop($1, Add, $3) }
|   lit MINUS multi_expr { Binop($1, Sub, $3) }

relational_expr:
	add_expr		{ $1 }
|   relational_expr LT relational_expr    { Binop($1, Less, $3) }
|   relational_expr LEQ relational_expr   { Binop($1, Leq, $3) }
|   relational_expr GT relational_expr    { Binop($1, Greater, $3) }
|   relational_expr GEQ relational_expr   { Binop($1, Geq, $3) }

equality_expr:
	relational_expr { $1 }
|   equality_expr EQ equality_expr    { Binop($1, Equal, $3) } 
|   equality_expr NEQ equality_expr   { Binop($1, Neq, $3) }

logical_AND_expr:
	equality_expr { $1 }
|	logical_AND_expr AND logical_AND_expr   { Binop($1, And, $3) }

logical_OR_expr:
	logical_AND_expr { $1 }
|   logical_AND_expr OR logical_OR_expr    { Binop($1, Or, $3) }

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */


arithmetic:
    lit PLUS int_term {Binop($1, Plus, $3)}
|	lit MINUS int_term {Binop($1, Minus, $3)}
|	int_term {$1}

int_term:
	int_term TIMES lit {Binop($1, Times, $3)}
|	int_term DIVIDE lit {Binop($1, Divide, $3)}
|	lit {$1}

/*atom:
	INT_LIT {IntLit($1)}
|	ID {Id($1)}*/

lit:
	INT_LIT {IntLit($1)}
|	note {$1}
|	ID {Id($1)}
|   STRING_LIT {String_Lit($1)}  /*why is this here? */

app_gen:

|	funk reg_list {FuncList($1, $2)}
|   reg_list  {BasicList($1)}

funk:
	LPAREN f_arithmetics RPAREN {$2}

f_arithmetics:
	f_arithmetics COMMA function_invocation {$3 :: $1}
| 	function_invocation {[$1]}

function_invocation:
	ID LPAREN funk_args RPAREN {FunkCall($1, List.rev $3)}
|	ID LPAREN RPAREN {FunkCall($1, [])}

funk_args:
	funk_args COMMA arithmeticID_arg {$3 :: $1}
|	arithmeticID_arg {[$1]}
/*|   STRING_LIT {String_Lit($1)}*/

arithmeticID_arg:
   /* {0} no clue why we'd have nothing */ 
	app_gen {$1}
   | arithmetic {$1}
   /*| arith {$1} */

reg_list:
	LBRACK funk_args RBRACK {List.rev $2}

note:
	INT_LIT PERIOD NOTE_TYPE {Note($1, $3)}
/*|   INT_LIT PERIOD S {Note($1, $3)}
|	INT_LIT PERIOD E {Note($1, $3)}
|	INT_LIT PERIOD Q {Note($1, $3)}
|	INT_LIT PERIOD H {Note($1, $3)}
|	INT_LIT PERIOD W {Note($1, $3)}*/
