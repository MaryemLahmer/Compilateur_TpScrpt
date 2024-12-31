type token =
  | INT of (
# 6 "parser.mly"
        int
# 6 "parser.mli"
)
  | BOOL of (
# 7 "parser.mly"
        bool
# 11 "parser.mli"
)
  | IDENT of (
# 8 "parser.mly"
        string
# 16 "parser.mli"
)
  | FUNCTION
  | RETURN
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | SEMICOLON
  | EQ
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | IF
  | ELSE
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
