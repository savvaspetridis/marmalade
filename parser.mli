type token =
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | SEMI
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | INT
  | NOTE
  | STRING
  | MEASURE
  | PHRASE
  | SONG
  | LIST
  | DIVIDE
  | ASSIGN
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | DASH
  | APPEND
  | NOT
  | NOTE_TYPE of (char)
  | IF
  | ELSE
  | ELIF
  | AND
  | OR
  | PERIOD
  | COLON
  | RETURN
  | WHILE
  | FUNK
  | AT
  | DOLLAR
  | INT_LIT of (int)
  | STRING_LIT of (string)
  | ID of (string)
  | INSTRUMENT of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
