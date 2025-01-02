type token =
  | INT of (
# 5 "parser.mly"
        int
# 6 "parser.mli"
)
  | BOOL of (
# 6 "parser.mly"
        bool
# 11 "parser.mli"
)
  | IDENT of (
# 7 "parser.mly"
        string
# 16 "parser.mli"
)
  | STRING of (
# 7 "parser.mly"
        string
# 21 "parser.mli"
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
  | WHILE
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
