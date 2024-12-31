type token =
  | INT of (
# 6 "parser.mly"
        int
# 6 "parser.ml"
)
  | BOOL of (
# 7 "parser.mly"
        bool
# 11 "parser.ml"
)
  | IDENT of (
# 8 "parser.mly"
        string
# 16 "parser.ml"
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

open Parsing
let _ = parse_error;;
# 3 "parser.mly"
  open Ast
# 38 "parser.ml"
let yytransl_const = [|
  260 (* FUNCTION *);
  261 (* RETURN *);
  262 (* LPAREN *);
  263 (* RPAREN *);
  264 (* LBRACE *);
  265 (* RBRACE *);
  266 (* SEMICOLON *);
  267 (* EQ *);
  268 (* PLUS *);
  269 (* MINUS *);
  270 (* TIMES *);
  271 (* DIV *);
  272 (* IF *);
  273 (* ELSE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\004\000\004\000\005\000\005\000\
\005\000\005\000\006\000\006\000\006\000\006\000\006\000\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\007\000\001\000\003\000\001\000\007\000\
\005\000\002\000\001\000\001\000\001\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\016\000\000\000\000\000\000\000\001\000\
\000\000\000\000\003\000\000\000\000\000\011\000\012\000\013\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\004\000\000\000\000\000\015\000\000\000\006\000\000\000\
\000\000\000\000\000\000\008\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\020\000\021\000\022\000"

let yysindex = "\002\000\
\005\255\000\000\013\255\000\000\019\000\023\255\024\255\000\000\
\005\255\032\255\000\000\027\255\255\254\000\000\000\000\000\000\
\022\255\022\255\034\255\033\255\031\255\035\255\035\255\019\255\
\022\255\000\000\255\254\022\255\000\000\020\255\000\000\035\255\
\255\254\026\255\255\254\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\044\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\036\255\003\255\012\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\255\
\000\000\028\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\037\000\000\000\021\000\001\000\245\255"

let yytablesize = 48
let yytable = "\014\000\
\015\000\016\000\001\000\017\000\018\000\023\000\024\000\014\000\
\003\000\014\000\014\000\007\000\007\000\030\000\019\000\007\000\
\032\000\014\000\008\000\007\000\010\000\010\000\014\000\015\000\
\016\000\029\000\033\000\018\000\010\000\010\000\028\000\028\000\
\009\000\034\000\013\000\036\000\009\000\009\000\012\000\025\000\
\027\000\026\000\035\000\002\000\005\000\011\000\028\000\031\000"

let yycheck = "\001\001\
\002\001\003\001\001\000\005\001\006\001\017\000\018\000\007\001\
\004\001\009\001\010\001\009\001\010\001\025\000\016\001\003\001\
\028\000\017\001\000\000\017\001\009\001\010\001\001\001\002\001\
\003\001\007\001\007\001\006\001\017\001\006\001\012\001\012\001\
\010\001\033\000\008\001\035\000\009\001\010\001\007\001\006\001\
\010\001\009\001\017\001\000\000\009\001\009\000\012\001\027\000"

let yynames_const = "\
  FUNCTION\000\
  RETURN\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  SEMICOLON\000\
  EQ\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  IF\000\
  ELSE\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  BOOL\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'declaration_list) in
    Obj.repr(
# 19 "parser.mly"
                         ( _1 )
# 146 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'declaration) in
    Obj.repr(
# 22 "parser.mly"
                ( [_1] )
# 153 "parser.ml"
               : 'declaration_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'declaration) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'declaration_list) in
    Obj.repr(
# 23 "parser.mly"
                                           ( _1 :: _3 )
# 161 "parser.ml"
               : 'declaration_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'instruction_list) in
    Obj.repr(
# 27 "parser.mly"
      ( DFunction (_2, [], None, _6) )
# 169 "parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'instruction) in
    Obj.repr(
# 30 "parser.mly"
                ( [_1] )
# 176 "parser.ml"
               : 'instruction_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'instruction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'instruction_list) in
    Obj.repr(
# 31 "parser.mly"
                                           ( _1 :: _3 )
# 184 "parser.ml"
               : 'instruction_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 34 "parser.mly"
               ( IExpr _1 )
# 191 "parser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'instruction) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'instruction) in
    Obj.repr(
# 35 "parser.mly"
                                                             ( IIf (_3, _5, Some _7) )
# 200 "parser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'instruction) in
    Obj.repr(
# 36 "parser.mly"
                                            ( IIf (_3, _5, None) )
# 208 "parser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 37 "parser.mly"
                      ( IReturn (Some _2) )
# 215 "parser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 39 "parser.mly"
        ( EConstInt _1 )
# 222 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 40 "parser.mly"
         ( EConstBool _1 )
# 229 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 41 "parser.mly"
          ( EIdent _1 )
# 236 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 42 "parser.mly"
                               ( EBinOp ("+", _1, _3) )
# 244 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 43 "parser.mly"
                             ( _2 )
# 251 "parser.ml"
               : 'expression))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
