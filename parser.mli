type token =
  | INT of (
# 7 "parser.mly"
        int
# 6 "parser.mli"
)
  | BOOL of (
# 8 "parser.mly"
        bool
# 11 "parser.mli"
)
  | IDENT of (
# 9 "parser.mly"
        string
# 16 "parser.mli"
)
  | STRING of (
# 9 "parser.mly"
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
