%{
  open Ast
  exception SyntaxError of string
%}


%token <int> INT
%token <bool> BOOL
%token <string> IDENT STRING
%token FUNCTION RETURN
%token LPAREN RPAREN LBRACE RBRACE
%token SEMICOLON EQ PLUS MINUS TIMES DIV
%token IF ELSE WHILE
%token EOF

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS       /* highest precedence */

%start program
%type <Ast.program> program

%%

program:
  | declarations EOF { $1 }
  | error EOF { raise (SyntaxError "Syntax error in program") }
;


declarations:
  | /* empty */ { [] }
  | declaration declarations { $1 :: $2 }
;

declaration:
  | FUNCTION IDENT LPAREN param_list RPAREN block 
    { DFunction($2, $4, None, $6) }
  | error { raise (SyntaxError "Invalid declaration") }
;

param_list:
  | /* empty */ { [] }
  | IDENT { [($1, None)] }
  | IDENT SEMICOLON param_list { ($1, None) :: $3 }
;

block:
  | LBRACE instruction_list RBRACE { $2 }
;

instruction_list:
  | /* empty */ { [] }
  | instruction instruction_list { $1 :: $2 }
  | error { raise (SyntaxError "Invalid instruction") }
;

instruction:
  | expression SEMICOLON { IExpr($1) }
  | RETURN expression SEMICOLON { IReturn(Some($2)) }
  | RETURN SEMICOLON { IReturn(None) }
  | IF LPAREN expression RPAREN instruction 
    { IIf($3, $5, None) }
  | IF LPAREN expression RPAREN instruction ELSE instruction 
    { IIf($3, $5, Some($7)) }
  | WHILE LPAREN expression RPAREN instruction
    { IWhile($3, $5) }
  | block { IBlock($1) }
  | error { raise (SyntaxError "Invalid Instruction") }

;

expression:
  | INT { EConstInt($1) }
  | BOOL { EConstBool($1) }
  | STRING { EConstString($1) }
  | IDENT { EIdent($1) }
  | expression PLUS expression { EBinOp("+", $1, $3) }
  | expression MINUS expression { EBinOp("-", $1, $3) }
  | expression TIMES expression { EBinOp("*", $1, $3) }
  | expression DIV expression { EBinOp("/", $1, $3) }
  | MINUS expression %prec UMINUS { EUnaryOp("-", $2) }
  | LPAREN expression RPAREN { $2 }
  | error { raise (SyntaxError "Invalid expression") }
;