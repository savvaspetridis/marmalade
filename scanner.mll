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
|   's'         { S }
|   'e'         { E }
|   'q'         { Q }
|   'h'         { H }
|   'w'         { W }
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
|   '@'         { AT }
|   '$'         { DOLLAR } 
|   (digit)+ as lxm { INT_LIT(int_of_string lxm) }
|   (letter)(letter | digit | '_')* as lxm { STRING_LIT(lxm) }
|   (letter)+ as lxm { INSTRUMENT(lxm) }
|   eof       { EOF }
|   _ as char { raise (Failure("illegal character: " ^ Char.escaped char)) }

and comment = parse
    "*/"        { token lexbuf }
|   _           { comment lexbuf }
