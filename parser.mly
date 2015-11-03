 %{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token SEMI COMMA PLUS MINUS TIMES
%token DIVIDE ASSIGN EQ NEQ LT LEQ
%token GT GEQ
%token IF ELSE ELIF AND OR CONT BREAK
%token INC DEC PERIOD COLON
%token RETURN FOR WHILE TO
%token FUNK
%token <int> INT_LIT
%token <string> STRING_LIT ID INSTRUMENT
%token EOF

%nonassoc ELSE
%right ASSIGN APPEND
%left LBRACK
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left IN NOT_IN
%left PLUS MINUS
%left TIMES DIVIDE 
%right NOT

%start program
%type <Ast.program> program

%%
program:
    /* nothing */ /*{ { stmts = []; funcs = [] } }*/ {{0; 0}}
    /* List is built backwards */
|   program fdecl /*{ { stmts = $1.stmts; funcs = $2::$1.funcs } } */ {{0; 0}}
|	program stmt  /*{ { stmts = $2::$1.stmts; funcs = $1.funcs } } */ {{0; 0}}

funkdecl:
	FUNK Id LPAREN param_list RPAREN LBRACE stmt_list RBRACE {0}

param_list:
	/* nothing */ { [] }
|	ID	/*{ [VarDecl($1, Id($2))] }*/ {0}
|	param_list COMMA ID /*{ VarDecl($3, Id($4))::$1}*/ {0}

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
	Id APPEND expr
|	Id ASSIGN expr

conditional_stmt:
	IF LPAREN expr RPAREN stmt elif_list %prec NOELSE /*{ If(($3,$5)::$6, Block([])) }*/ {0}
|	IF LPAREN expr RPAREN stmt elif_list ELSE stmt /*{ If(($3,$5)::$6, $8) }*/ {0}

elif_list:
	/* nothing */ { [] }
|	elif_list ELIF LPAREN expr RPAREN stmt /*{ ($4, $6)::$1 }*/ {0}

expr:
	app_gen 
|	val
|	boolean
|	LPAREN expr RPAREN

arithmetic:
	lit PLUS int_term
|	lit MINUS int_term
|	int_term

int_term:
	int_term TIMES atom
|	int_term DIVIDE atom
|	atom

atom:
	INT_LIT
|	Id

lit:
	INT_LIT
|	note
|	Id

boolean:
	expr AND expr
|	exp OR exp
|	val GT val
|	val LT val
|	val LEQ val
|	val GEQ val
|	val EQ val
| 	val NEQ val
| 	NOT val

val:
	arithmetic
|	Id

app_gen:
	app_s
|	app_phrases
|	app_measure
|	app_note

app_s:
	song_exp APPEND app_phrases 
|	song_exp

app_phrases:
	app_phrases APPEND phrase_branch
|	phrase_conglomorate

phrase_branch:
	phrase_exp APPEND app_measure
|	phrase_exp

app_measure:
	measure_exp APPEND note_regex
|	measure_exp

song_exp:
	tempo funk songvals 
|	temp songvals
|	funk songvals 
|	song

phrase_exp:
	instruments	funk phrase_vals
|	instruments phrase_vals
|	funk phrase_vals
|	phrase_vals

measure_exp:
	t_sig funk measure_vals
|	t_sig measure_vals
|	funk measure_vals
|	measure_vals

funk:
	LPAREN f_vals RPAREN

f_vals:
	f_vals COMMA function_invocation

function_invocation:
	Id LPAREN funk_args RPAREN

funk_args:
	funk_args COMMA valid_arg
|	valid_arg

valid_arg:
	app_gen
|	val

 















