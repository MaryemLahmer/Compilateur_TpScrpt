type token =
  | INT of (
# 7 "parser.mly"
        int
# 6 "parser.ml"
)
  | BOOL of (
# 8 "parser.mly"
        bool
# 11 "parser.ml"
)
  | IDENT of (
# 9 "parser.mly"
        string
# 16 "parser.ml"
)
  | STRING of (
# 9 "parser.mly"
        string
# 21 "parser.ml"
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

open Parsing
let _ = parse_error;;
# 2 "parser.mly"
  open Ast
  exception SyntaxError of string
# 45 "parser.ml"
let yytransl_const = [|
  261 (* FUNCTION *);
  262 (* RETURN *);
  263 (* LPAREN *);
  264 (* RPAREN *);
  265 (* LBRACE *);
  266 (* RBRACE *);
  267 (* SEMICOLON *);
  268 (* EQ *);
  269 (* PLUS *);
  270 (* MINUS *);
  271 (* TIMES *);
  272 (* DIV *);
  273 (* IF *);
  274 (* ELSE *);
  275 (* WHILE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* IDENT *);
  260 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\004\000\004\000\
\004\000\005\000\006\000\006\000\006\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\000\000"

let yylen = "\002\000\
\002\000\002\000\000\000\002\000\006\000\001\000\000\000\001\000\
\003\000\003\000\000\000\002\000\001\000\002\000\003\000\002\000\
\005\000\007\000\005\000\001\000\001\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\002\000\003\000\001\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\033\000\000\000\000\000\002\000\
\000\000\001\000\006\000\004\000\000\000\000\000\000\000\000\000\
\000\000\009\000\000\000\005\000\000\000\022\000\023\000\025\000\
\024\000\000\000\000\000\000\000\000\000\000\000\020\000\000\000\
\000\000\000\000\032\000\016\000\000\000\000\000\030\000\000\000\
\000\000\010\000\012\000\014\000\000\000\000\000\000\000\000\000\
\015\000\031\000\000\000\000\000\000\000\000\000\028\000\029\000\
\000\000\000\000\000\000\000\000\019\000\000\000\018\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\015\000\031\000\032\000\033\000\034\000"

let yysindex = "\011\000\
\011\255\000\000\018\000\025\255\000\000\031\000\071\255\000\000\
\028\255\000\000\000\000\000\000\045\255\040\255\047\255\045\255\
\052\255\000\000\083\255\000\000\000\000\000\000\000\000\000\000\
\000\000\123\255\003\255\003\255\061\255\068\255\000\000\081\255\
\083\255\142\255\000\000\000\000\148\255\080\255\000\000\003\255\
\003\255\000\000\000\000\000\000\003\255\003\255\003\255\003\255\
\000\000\000\000\100\255\125\255\113\255\113\255\000\000\000\000\
\103\255\103\255\000\000\060\255\000\000\103\255\000\000"

let yyrindex = "\000\000\
\081\000\000\000\074\255\000\000\000\000\000\000\081\000\000\000\
\000\000\000\000\000\000\000\000\090\255\091\255\000\000\090\255\
\000\000\000\000\101\255\000\000\043\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\101\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\134\255\138\255\000\000\000\000\
\000\000\000\000\023\255\063\255\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\094\000\000\000\102\000\104\000\086\000\207\255\230\255"

let yytablesize = 164
let yytable = "\037\000\
\038\000\039\000\035\000\022\000\023\000\024\000\025\000\060\000\
\061\000\027\000\003\000\001\000\063\000\051\000\052\000\004\000\
\028\000\008\000\053\000\054\000\055\000\056\000\021\000\021\000\
\021\000\021\000\021\000\009\000\021\000\021\000\010\000\021\000\
\021\000\032\000\013\000\032\000\021\000\032\000\032\000\021\000\
\021\000\021\000\021\000\021\000\021\000\021\000\021\000\014\000\
\021\000\021\000\016\000\021\000\013\000\032\000\017\000\032\000\
\021\000\032\000\032\000\021\000\019\000\021\000\017\000\017\000\
\017\000\017\000\017\000\040\000\017\000\017\000\011\000\017\000\
\017\000\006\000\041\000\004\000\017\000\062\000\006\000\017\000\
\003\000\017\000\021\000\022\000\023\000\024\000\025\000\050\000\
\026\000\027\000\042\000\019\000\045\000\046\000\047\000\048\000\
\028\000\007\000\008\000\029\000\012\000\030\000\059\000\022\000\
\023\000\024\000\025\000\057\000\026\000\027\000\011\000\019\000\
\045\000\046\000\047\000\048\000\028\000\018\000\043\000\029\000\
\020\000\030\000\035\000\022\000\023\000\024\000\025\000\047\000\
\048\000\027\000\000\000\000\000\058\000\036\000\000\000\000\000\
\028\000\045\000\046\000\047\000\048\000\026\000\000\000\000\000\
\026\000\027\000\026\000\026\000\027\000\000\000\027\000\027\000\
\044\000\000\000\045\000\046\000\047\000\048\000\049\000\000\000\
\045\000\046\000\047\000\048\000"

let yycheck = "\026\000\
\027\000\028\000\000\001\001\001\002\001\003\001\004\001\057\000\
\058\000\007\001\000\001\001\000\062\000\040\000\041\000\005\001\
\014\001\000\000\045\000\046\000\047\000\048\000\000\001\001\001\
\002\001\003\001\004\001\003\001\006\001\007\001\000\000\009\001\
\010\001\011\001\007\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\000\001\001\001\002\001\003\001\004\001\003\001\
\006\001\007\001\011\001\009\001\010\001\011\001\008\001\013\001\
\014\001\015\001\016\001\017\001\009\001\019\001\000\001\001\001\
\002\001\003\001\004\001\007\001\006\001\007\001\000\001\009\001\
\010\001\000\001\007\001\005\001\014\001\018\001\005\001\017\001\
\000\000\019\001\000\001\001\001\002\001\003\001\004\001\008\001\
\006\001\007\001\010\001\009\001\013\001\014\001\015\001\016\001\
\014\001\008\001\008\001\017\001\007\000\019\001\000\001\001\001\
\002\001\003\001\004\001\008\001\006\001\007\001\010\001\009\001\
\013\001\014\001\015\001\016\001\014\001\016\000\033\000\017\001\
\017\000\019\001\000\001\001\001\002\001\003\001\004\001\015\001\
\016\001\007\001\255\255\255\255\008\001\011\001\255\255\255\255\
\014\001\013\001\014\001\015\001\016\001\008\001\255\255\255\255\
\011\001\008\001\013\001\014\001\011\001\255\255\013\001\014\001\
\011\001\255\255\013\001\014\001\015\001\016\001\011\001\255\255\
\013\001\014\001\015\001\016\001"

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
  WHILE\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  BOOL\000\
  IDENT\000\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'declarations) in
    Obj.repr(
# 26 "parser.mly"
                     ( _1 )
# 202 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 27 "parser.mly"
              ( raise (SyntaxError "Syntax error in program") )
# 208 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 32 "parser.mly"
                ( [] )
# 214 "parser.ml"
               : 'declarations))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'declaration) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'declarations) in
    Obj.repr(
# 33 "parser.mly"
                             ( _1 :: _2 )
# 222 "parser.ml"
               : 'declarations))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'param_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 38 "parser.mly"
    ( DFunction(_2, _4, None, _6) )
# 231 "parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "parser.mly"
          ( raise (SyntaxError "Invalid declaration") )
# 237 "parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "parser.mly"
                ( [] )
# 243 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 44 "parser.mly"
          ( [(_1, None)] )
# 250 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param_list) in
    Obj.repr(
# 45 "parser.mly"
                               ( (_1, None) :: _3 )
# 258 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'instruction_list) in
    Obj.repr(
# 49 "parser.mly"
                                   ( _2 )
# 265 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
                ( [] )
# 271 "parser.ml"
               : 'instruction_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instruction) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'instruction_list) in
    Obj.repr(
# 54 "parser.mly"
                                 ( _1 :: _2 )
# 279 "parser.ml"
               : 'instruction_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
          ( raise (SyntaxError "Invalid instruction") )
# 285 "parser.ml"
               : 'instruction_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 59 "parser.mly"
                         ( IExpr(_1) )
# 292 "parser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 60 "parser.mly"
                                ( IReturn(Some(_2)) )
# 299 "parser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
                     ( IReturn(None) )
# 305 "parser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'instruction) in
    Obj.repr(
# 63 "parser.mly"
    ( IIf(_3, _5, None) )
# 313 "parser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'instruction) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'instruction) in
    Obj.repr(
# 65 "parser.mly"
    ( IIf(_3, _5, Some(_7)) )
# 322 "parser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'instruction) in
    Obj.repr(
# 67 "parser.mly"
    ( IWhile(_3, _5) )
# 330 "parser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 68 "parser.mly"
          ( IBlock(_1) )
# 337 "parser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "parser.mly"
          ( raise (SyntaxError "Invalid Instruction") )
# 343 "parser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 74 "parser.mly"
        ( EConstInt(_1) )
# 350 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 75 "parser.mly"
         ( EConstBool(_1) )
# 357 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 76 "parser.mly"
           ( EConstString(_1) )
# 364 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 77 "parser.mly"
          ( EIdent(_1) )
# 371 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 78 "parser.mly"
                               ( EBinOp("+", _1, _3) )
# 379 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 79 "parser.mly"
                                ( EBinOp("-", _1, _3) )
# 387 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 80 "parser.mly"
                                ( EBinOp("*", _1, _3) )
# 395 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 81 "parser.mly"
                              ( EBinOp("/", _1, _3) )
# 403 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 82 "parser.mly"
                                  ( EUnaryOp("-", _2) )
# 410 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 83 "parser.mly"
                             ( _2 )
# 417 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
          ( raise (SyntaxError "Invalid expression") )
# 423 "parser.ml"
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
