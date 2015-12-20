 %{ open Ast 

let scope_id = ref 1

let inc_block_id (u:unit) =
    let x = scope_id.contents in
    scope_id := x + 1; x
%}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token SEMI COMMA PLUS MINUS TIMES 
%token <char> BOUND
%token INT NOTE STRING MEASURE PHRASE SONG LIST TIMESIG INSTR TEMPO INTLIST STRL
%token DIVIDE ASSIGN EQ NEQ LT LEQ
%token GT GEQ DASH APPEND NOT
%token <char> NOTE_TYPE
%token IF ELSE ELIF AND OR
%token PERIOD COLON
%token RETURN WHILE
%token FUNK AT DOLLAR INDEX
%token <int> INT_LIT
%token <string> STRING_LIT ID INSTRUMENT
%token EOF

%nonassoc ELSE
%nonassoc NOELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE 
%right NOT

%start program
%type <Ast.program> program

%%
program:
    /* nothing */  {{stmts = []; funcs = []} }
    /* List is built backwards */
|   program fdecl {{ stmts = $1.stmts; funcs = $2 :: $1.funcs }} 
|	program stmt {{stmts = $2 :: $1.stmts; funcs = $1.funcs}} 

/* Function Declaration */ 

fdecl:
   t_dec_l ID LPAREN arguments RPAREN LBRACE stmt_list RBRACE
     {{ ret_type = List.hd (List.rev $1);
     	f_type = List.tl (List.rev $1); 
        fname = $2;
	    args = $4;
	    body = {locals = $4; statements = List.rev $7; block_id = inc_block_id ()} 
	 }}

t_dec_l:
  		FUNK  { [] }
    | t_dec_l type_dec { $2 :: $1 } 

/* Argument list creator for Function Declarations */

arguments: 
	/* nothing */ { [] }
	| arg_list    { List.rev $1 }

/* Argument list */

arg_list:
	fvmod { [$1] }
	| arg_list COMMA fvmod { $3 :: $1 }

fvmod:
	INT ID {($2, false, Int)}
	| STRING ID {($2, false, String)}
	| NOTE ID {($2, false, Note)}
	| SONG ID {($2, true, Song)}
	| MEASURE ID {($2, true, Measurepoo)}
	| INTLIST ID {($2, true, Intlist)}
	| STRL ID {($2, true, Stringlist)}
	| PHRASE ID {($2, true, Phrase)}
    | TIMESIG ID {($2, false, TimeSig)}
    | INSTR ID {($2, false, Instr)}
    | TEMPO ID {($2, false, Tempo)}

/* a stmt can be an expression, variable modification, a conditional stmt, or return */

stmt:
	expr SEMI { Expr($1) }
|	vmod SEMI	  { VarDecl($1) }
|	conditional_stmt { $1 }
| 	RETURN expr SEMI { Return($2)}

/* all type declarations */

type_dec:
	INT 	{Int}
|	NOTE 	{Note}
|	MEASURE	{Measurepoo}
|	PHRASE	{Phrase}
|	SONG	{Song}
|	STRING 	{String}
| 	LIST 	{List}
|	INTLIST	{Intlist}
|	STRL 	{Stringlist}
|   TIMESIG {TimeSig}
|   INSTR   {Instr}
|   TEMPO   {Tempo}

conditional_stmt:
	IF LPAREN expr RPAREN block %prec NOELSE { If($3, $5, {locals = []; statements = []; block_id = inc_block_id ()}) }
| 	IF LPAREN expr RPAREN block ELSE block { If($3, $5, $7) }
|	WHILE LPAREN expr RPAREN block { While($3, $5) }

block:
	LBRACE stmt_list RBRACE { {locals = []; statements = List.rev $2; block_id = inc_block_id ()} }

stmt_list:
    /* nothing */  { [] }
    | stmt_list stmt { $2 :: $1 } 
	
vmod:
|	type_dec ID ASSIGN expr {Assign($1, $2, $4)}
|	ID ASSIGN expr {Update($1, $3)}


expr:
app_gen  {$1}
| list_index {$1}
| arith {$1}
| AT LPAREN special_expression RPAREN /*{Regex({$3.ids; $3.bounds})}*/ {Regex($3)}
| add_on_expr {$1}


list_index:
    ID INDEX INT_LIT { Index($1, IntLit($3)) }

add_on_expr:
  DOLLAR LPAREN RPAREN reg_list { Measure($4, TimeSig(4, 4))}
|	DOLLAR DOLLAR LPAREN RPAREN reg_list  { Phrase( $5, Instr("PIANO")) }
|	DOLLAR DOLLAR DOLLAR LPAREN RPAREN reg_list { Song($6, Tempo(60))}
|   DOLLAR LPAREN INT_LIT COLON INT_LIT RPAREN reg_list { Measure($7, TimeSig($3, $5)) }  
|   DOLLAR LPAREN ID RPAREN reg_list { Phrase( $5, Instr($3))  }
|   DOLLAR LPAREN INT_LIT RPAREN reg_list { Song( $5, Tempo($3)) }

/* beginning of chain of expressions, ordered by precedence */ 

arith: 
	l_OR { $1 }

primary_expr:
    ID              { Id($1) }
|	literal 		{ $1 }
|	LPAREN expr RPAREN { $2 }

special_expression:
						{{ids = []; bounds = []}}
|	special_expression ID {{ids = $2 :: $1.ids; bounds = $1.bounds}}
|	special_expression LBRACE bound_list RBRACE {{ids = $1.ids; bounds = $3 :: $1.bounds}}

bound_list:
			{[]}
|	bound_list BOUND DASH BOUND {(Ranges($2, $4) :: $1)}

literal:
	INT_LIT {IntLit($1)}
|	note 			{$1}
|   STRING_LIT {String_Lit($1)} 
|	DOLLAR function_invocation { $2 }

/* multiplication */

mul_expr:
	primary_expr /*lit*/		{ $1 }
|	mul_expr TIMES primary_expr { Binop($1, Times,$3) }
|   mul_expr DIVIDE primary_expr { Binop($1, Divide, $3) } 

/* addition */

add_expr:
	mul_expr { $1 }
|	primary_expr PLUS mul_expr  { Binop($1, Plus, $3) }
|   primary_expr MINUS mul_expr { Binop($1, Minus, $3) }

/* <, <=, >, >= */

r_expr:
	add_expr		{ $1 }
|   r_expr LT r_expr    { Binop($1, Less, $3) }
|   r_expr LEQ r_expr   { Binop($1, Leq, $3) }
|   r_expr GT r_expr    { Binop($1, Greater, $3) }
|   r_expr GEQ r_expr   { Binop($1, Geq, $3) }

/* equal and not equal */

eq_exp:
	r_expr { $1 }
|   eq_exp EQ eq_exp    { Binop($1, Equal, $3) } 
|   eq_exp NEQ eq_exp   { Binop($1, Neq, $3) }

/* logical And */

l_AND:
	eq_exp { $1 }
|	l_AND AND l_AND   { Binop($1, And, $3) }

/* logical Or */

l_OR:
	l_AND { $1 }
|   l_AND OR l_OR   { Binop($1, Or, $3) }

/* app_gen creates lists, as well as lists of functions for application */

app_gen: 
|	funk reg_list {FuncList($1, $2)}
|   reg_list  {BasicList($1)}

/* parenthesis contain the list of functions to be applied */

funk:
	LPAREN f_arithmetics RPAREN {$2}

/* make list of function_invocations */

f_arithmetics:
	f_arithmetics COMMA function_invocation {$3 :: $1}
| 	function_invocation {[$1]}

/* ID and arguments of function in list */

function_invocation:
	ID LPAREN funk_args RPAREN {FunkCall($1, List.rev $3)}
|	ID LPAREN RPAREN {FunkCall($1, [])}

/* make list of function arguments */

funk_args:
	funk_args COMMA arithmeticID_arg {$3 :: $1}
|	arithmeticID_arg {[$1]}

/* arguments can be many expressions: another list, addition expr, logical expr, etc */

arithmeticID_arg:
    list_index {$1}
    | app_gen {$1}
    | arith {$1}
    | add_on_expr { $1 }
    | function_invocation { $1 }


/*reg_list_list_wrapper:
	LBRACK reg_list_list RBRACK {List.rev $2} 

reg_list_list:
	reg_list_list COMMA reg_list {$3 :: $1}
|	reg_list_list COMMA ID {[Id($3)] :: $1}
| reg_list {[$1]} 

reg_list_list_list_wrapper:
	LBRACK reg_list_list_list RBRACK {List.rev $2}

reg_list_list_list:
	reg_list_list_list COMMA reg_list_list_wrapper {$3 :: $1}
|	reg_list_list_list COMMA ID {[[Id($3)]] :: $1}
| 	reg_list_list_wrapper {[$1]}*/



reg_list:
	LBRACK funk_args RBRACK {List.rev $2}

note:
	INT_LIT NOTE_TYPE {Note($1, $2)}
/*|   INT_LIT PERIOD S {Note($1, $3)}
|	INT_LIT PERIOD E {Note($1, $3)}
|	INT_LIT PERIOD Q {Note($1, $3)}
|	INT_LIT PERIOD H {Note($1, $3)}
|	INT_LIT PERIOD W {Note($1, $3)}*/
