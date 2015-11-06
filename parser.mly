 %{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token SEMI COMMA PLUS MINUS TIMES
%token DIVIDE ASSIGN EQ NEQ LT LEQ
%token GT GEQ DASH APPEND NOT
%token S E Q H W
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
    /* nothing */ /*{ { stmts = []; funcs = [] } }*/ {{0; 0}}
    /* List is built backwards */
|	program stmt  /*{ { stmts = $2::$1.stmts; funcs = $1.funcs } } */ {{0; 0}}

stmt:
	expr SEMI /*{ Expr($1) }*/ {0}
|	vmod SEMI	  /*{ VarDeclS($1) }*/ {0}

vmod:
	ID ASSIGN expr {0}

expr:
	app_gen  {0}
|	arithmetic {0}
	
arithmetic:
    lit PLUS int_term {0}
|	lit MINUS int_term {0}
|	int_term {0}

int_term:
	int_term TIMES atom {0}
|	int_term DIVIDE atom {0}
|	atom {0}

atom:
	INT_LIT {0}
|	ID {0}

lit:
	INT_LIT {0}
|	note {0}
|	ID {0}
|   STRING_LIT {0}

app_gen:
	funk reg_list {0}
|   reg_list {0}

funk:
	LPAREN f_arithmetics RPAREN {0}

f_arithmetics:
	f_arithmetics COMMA function_invocation {0}
| function_invocation {0}

function_invocation:
	ID LPAREN funk_args RPAREN {0}

funk_args:
	funk_args COMMA arithmeticID_arg {0}
|	arithmeticID_arg {0}
|   STRING_LIT {0}

arithmeticID_arg:
    {0}
|	app_gen {0}
|	arithmetic {0}

reg_list:
	LBRACK funk_args RBRACK {0}

note:
	INT_LIT PERIOD S {0}
|	INT_LIT PERIOD E {0}
|	INT_LIT PERIOD Q {0}
|	INT_LIT PERIOD H {0}
|	INT_LIT PERIOD W {0}
