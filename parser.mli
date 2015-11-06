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
  | S
  | E
  | Q
  | H
  | W
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
  | INSTRUMENT of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
