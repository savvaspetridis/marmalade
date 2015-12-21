{ open Parser }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
    [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
|   "/*"        { comment lexbuf }
|   '('         { LPAREN }
|   ')'         { RPAREN }
|   '{'         { LBRACE }
|   '}'         { RBRACE }
|   '['         { LBRACK }
|   ']'         { RBRACK }
|   ';'         { SEMI }
|   ','         { COMMA }
|   '+'         { PLUS }
|   '-'         { MINUS }
|   '*'         { TIMES }
|   '/'         { DIVIDE }
|   '='         { ASSIGN }
|   "=="        { EQ }
|   "!="        { NEQ }
|   '<'         { LT }
|   "<="        { LEQ }
|   '>'         { GT }
|   ">="        { GEQ }
|   '-'         { DASH }
|   "<<"        { APPEND } 
|   '!'         { NOT }
|   "if"        { IF }
|   "else"      { ELSE }
|   "elif"      { ELIF }
|   "and"       { AND }
|   "or"        { OR }
|   '.'         { PERIOD }
|   ':'         { COLON }
|   "return"    { RETURN }
|   "while"     { WHILE }
|   "funk"      { FUNK }
| 	"int"    	{ INT }                      
| 	"note"		{ NOTE}
|	"int_list"	{ INTLIST }
|	"str_list"	{ STRL}
| 	"string" 	{ STRING }
|	"measure" 	{ MEASURE }
|	"phrase"	{ PHRASE }
|	"song"		{ SONG }
|	"list"		{ LIST }
|   "timesig"   { TIMESIG }
|   "instr"     { INSTR }
|   "tempo"     { TEMPO }
|   '@'         { AT }
|   '&'         { INDEX }
|   '$'         { DOLLAR }
|   '.' (('s'|'e'|'q'|'h'|'w') as lxm) { NOTE_TYPE(lxm) }
|    (digit)+ as lxm { INT_LIT(int_of_string lxm) }
|   '"' ((letter | digit | '_' | ' ' | '.' | ',' |'!' | '$' | ':' | ';' |'(' | ')' | '[' | ']' | '{' |'}' | '&' | '&' | '#' | '@' | '?' | '<' | '>' | '+' | '=' | '-'  )* as lxm) '"' { STRING_LIT(lxm) }
|   (letter | digit | '_')+ as lxm  { ID(lxm) }
|	''' ((letter | digit | ' ') as lxm) ''' { BOUND(lxm) }
|   (letter)+ as lxm { INSTRUMENT(lxm) }
|   eof       { EOF }
|   _ as char { raise (Failure("Error: Illegal character: " ^ Char.escaped char)) }

and comment = parse
    "*/"        { token lexbuf }
|   _           { comment lexbuf }
