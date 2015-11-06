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
%token <string> STRING_LIT INSTRUMENT
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
	FUNK STRING_LIT LPAREN param_list RPAREN LBRACE stmt_list RBRACE {0}

param_list:
	/* nothing */ { [] }
|	STRING_LIT	/*{ [VarDecl($1, STRING_LIT($2))] }*/ {0}
|	param_list COMMA STRING_LIT /*{ VarDecl($3, STRING_LIT($4))::$1}*/ {0}

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
	STRING_LIT APPEND expr {0}
|	STRING_LIT ASSIGN expr {0}

conditional_stmt:
	IF LPAREN expr RPAREN stmt elif_list %prec NOELSE /*{ If(($3,$5)::$6, Block([])) }*/ {0}
|	IF LPAREN expr RPAREN stmt elif_list ELSE stmt /*{ If(($3,$5)::$6, $8) }*/ {0}

elif_list:
	/* nothing */ { [] }
|	elif_list ELIF LPAREN expr RPAREN stmt /*{ ($4, $6)::$1 }*/ {0}

expr:
	app_gen  {0}
|	arithmetic {0}
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
|	STRING_LIT {0}

lit:
	INT_LIT {0}
|	note {0}
|	STRING_LIT {0}

boolean:
	expr AND expr {0}
|	expr OR expr {0}
|	arithmetic GT arithmetic {0}
|	arithmetic LT arithmetic {0}
|	arithmetic LEQ arithmetic {0}
|	arithmetic GEQ arithmetic {0}
|	arithmetic EQ arithmetic {0}
| 	arithmetic NEQ arithmetic {0}
| 	NOT arithmetic {0}



app_gen:
	app_s {0}


app_s:
	song_exp APPEND song_exp {0} /*app_phrases {0}*/
|	song_exp {0}



song_exp:
	mod_data_type funk regex {0}
|	mod_data_type regex {0}
|	funk regex {0}
|	regex {0}



mod_data_type:
	tempo {0}
|	t_sig {0}
|	instruments {0}

funk:
	LPAREN f_arithmetics RPAREN {0}

f_arithmetics:
	f_arithmetics COMMA function_invocation {0}
| function_invocation {0}

function_invocation:
	STRING_LIT LPAREN funk_args RPAREN {0}

funk_args:
	funk_args COMMA arithmeticID_arg {0}
|	arithmeticID_arg {0}

arithmeticID_arg:
	app_gen {0}
|	arithmetic {0}

regex:
	AT LBRACE special_exp RBRACE {0}
|	LBRACK funk_args RBRACK {0}

special_exp:
	STRING_LIT LPAREN indicies RPAREN special_exp {0}
|	STRING_LIT {0}

indicies:
	indicies COMMA range {0}
|	indicies COMMA indivIDual {0}
| 	range {0}
|	indivIDual {0}

range:
	STRING_LIT INT_LIT STRING_LIT DASH STRING_LIT INT_LIT STRING_LIT {0}

indivIDual:
	STRING_LIT INT_LIT STRING_LIT {0}


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



	


