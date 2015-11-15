 %{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token SEMI COMMA PLUS MINUS TIMES
%token INT NOTE STRING MEASURE PHRASE SONG LIST
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
    /* nothing */ /*{ { stmts = []; funcs = [] } }*/ {{stmts = []; funcs = []} }
    /* List is built backwards */
|	program stmt  /*{ { stmts = $2::$1.stmts; funcs = $1.funcs } } */ {{stmts = $2 :: fst $1.stmts; funcs = $1.funcs}} /* statement head list which is program */

stmt:
	expr SEMI { Expr($1) }
|	vmod SEMI	  { VarDecl($1) }

type_dec:
	INT 	{Int}
|	NOTE 	{Note}
|	MEASURE	{Measure}
|	PHRASE	{Phrase}
|	SONG	{Song}
|	STRING 	{String}
| 	LIST 	{List}

vmod:
	type_dec ID ASSIGN expr {Assign($1, $2, $4)}

expr:
	app_gen  {$1}
|	arithmetic {$1}
	
arithmetic:
    lit PLUS int_term {Binop($1, Plus, $3)}
|	lit MINUS int_term {Binop($1, Minus, $3)}
|	int_term {$1}

int_term:
	int_term TIMES lit {Binop($1, Times, $3)}
|	int_term DIVIDE lit {Binop($1, Divide, $3)}
/*|	atom {$1}*/
|	lit {$1}

/*atom:
	INT_LIT {IntLit($1)}
|	ID {Id($1)}*/

lit:
	INT_LIT {IntLit($1)}
|	note {$1}
|	ID {Id($1)}
/*|   STRING_LIT {StringLit($1)}  why is this here? */

app_gen:
	funk reg_list {FuncList($1, $2)}
|   reg_list  {BasicList($1)}

funk:
	LPAREN f_arithmetics RPAREN {$2}

f_arithmetics:
	f_arithmetics COMMA function_invocation {$3 :: $1}
| 	function_invocation {[$1]}

function_invocation:
	ID LPAREN funk_args RPAREN {FunkCall($1, List.rev $3)}

funk_args:
	funk_args COMMA arithmeticID_arg {$3 :: $1}
|	arithmeticID_arg {[$1]}
/*|   STRING_LIT {String_Lit($1)}*/

arithmeticID_arg:
   /* {0} no clue why we'd have nothing */
	app_gen {$1}
|	arithmetic {$1}

reg_list:
	LBRACK funk_args RBRACK {List.rev $2}

note:
	INT_LIT PERIOD NOTE_TYPE {Note($1, $3)}
/*|   INT_LIT PERIOD S {Note($1, $3)}
|	INT_LIT PERIOD E {Note($1, $3)}
|	INT_LIT PERIOD Q {Note($1, $3)}
|	INT_LIT PERIOD H {Note($1, $3)}
|	INT_LIT PERIOD W {Note($1, $3)}*/
