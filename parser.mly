 %{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token SEMI COMMA PLUS MINUS TIMES
%token DIVIDE ASSIGN EQ NEQ LT LEQ
%token GT GEQ DASH APPEND NOT
%token S E Q H W
%token IF ELSE ELIF AND OR
%token PERIOD COLON
%token RETURN WHILE
%token FUNK AT DOLLAR SQU
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
|   program funkdecl /*{ { stmts = $1.stmts; funcs = $2::$1.funcs } } */ {{0; 0}}
|	program stmt  /*{ { stmts = $2::$1.stmts; funcs = $1.funcs } } */ {{0; 0}}

funkdecl:
	FUNK ID LPAREN param_list RPAREN LBRACE stmt_list RBRACE {0}

param_list:
	/* nothing */ { [] }
|	ID	/*{ [VarDecl($1, ID($2))] }*/ {0}
|	param_list COMMA ID /*{ VarDecl($3, ID($4))::$1}*/ {0}

stmt_list:
	/* nothing */ { [] }
|	stmt_list stmt /*{ $2::$1 }*/ {0}

stmt:
	expr SEMI /*{ Expr($1) }*/ {0}
|	RETURN expr SEMI /*{ Return($2) }*/ {0}
|	LBRACE stmt_list RBRACE	/*{ Block(List.rev $2) }*/ {[]}
|	conditional_stmt		/*{ $1 }*/ {0}
|	WHILE expr stmt /*{ While($2, $3) }*/ {0}
|	vmod SEMI	  /*{ VarDeclS($1) }*/ {0}

vmod:
	ID APPEND expr {0}
|	ID ASSIGN expr {0}

conditional_stmt:
	IF LPAREN expr RPAREN stmt elif_list %prec NOELSE /*{ If(($3,$5)::$6, Block([])) }*/ {0}
|	IF LPAREN expr RPAREN stmt elif_list ELSE stmt /*{ If(($3,$5)::$6, $8) }*/ {0}

elif_list:
	/* nothing */ { [] }
|	elif_list ELIF LPAREN expr RPAREN stmt /*{ ($4, $6)::$1 }*/ {0}

expr:
	app_gen  {0}
|	value {0}
|	boolean {0}
|	LPAREN expr RPAREN {0}

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

boolean:
	expr AND expr {0}
|	expr OR expr {0}
|	value GT value {0}
|	value LT value {0}
|	value LEQ value {0}
|	value GEQ value {0}
|	value EQ value {0}
| 	value NEQ value {0}
| 	NOT value {0}

value:
	arithmetic {0}
|	ID {0}

app_gen:
	app_s {0}
|	app_phrases {0}
|	app_measure {0}
|	app_note {0}

app_s:
	song_exp APPEND app_phrases {0}
|	song_exp {0}

app_phrases:
	app_phrases APPEND phrase_branch {0}
|	phrase_branch {0}

phrase_branch:
	phrase_exp APPEND app_measure {0}
|	phrase_exp {0}

app_measure:
	measure_exp APPEND app_note {0}
|	measure_exp {0}

song_exp:
	tempo funk regex {0}
|	tempo regex {0}
|	funk regex {0}
|	regex {0}

app_note:
	funk regex {0}
|	regex {0}

phrase_exp:
	instruments	funk regex {0}
|	instruments regex {0}
|	funk regex {0}
|	regex {0}

measure_exp:
	t_sig funk regex {0}
|	t_sig regex {0}
|	funk regex {0}
|	regex {0}

funk:
	LPAREN f_values RPAREN {0}

f_values:
	f_values COMMA function_invocation {0}
| function_invocation {0}

function_invocation:
	ID LPAREN funk_args RPAREN {0}

funk_args:
	funk_args COMMA valueID_arg {0}
|	valueID_arg {0}

valueID_arg:
	app_gen {0}
|	value {0}

regex:
	AT LBRACE special_exp RBRACE {0}

special_exp:
	STRING_LIT LPAREN indicies RPAREN special_exp {0}
|	STRING_LIT {0}

indicies:
	indicies COMMA range {0}
|	indicies COMMA indivIDual {0}
| 	range {0}
|	indivIDual {0}

range:
	SQU INT_LIT SQU DASH SQU INT_LIT SQU {0}

indivIDual:
	SQU INT_LIT SQU {0}
/* t_sig instrument tempo */

t_sig:
	DOLLAR LPAREN INT_LIT COLON INT_LIT LPAREN {0}

tempo:
	DOLLAR LPAREN INT_LIT LPAREN {0}

instruments:
	DOLLAR LPAREN INSTRUMENT LPAREN {0}

note:
	INT_LIT PERIOD S {0}
|	INT_LIT PERIOD E {0}
|	INT_LIT PERIOD Q {0}
|	INT_LIT PERIOD H {0}
|	INT_LIT PERIOD W {0}



	

















