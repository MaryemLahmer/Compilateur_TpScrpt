(*Grammaire  de TpScrpt*)
%{
  open Ast
%}

%token <int> INT
%token <bool> BOOL
%token <string> IDENT
%token FUNCTION RETURN LPAREN RPAREN LBRACE RBRACE SEMICOLON
%token EQ PLUS MINUS TIMES DIV
%token IF ELSE EOF

%start program
%type <Ast.program> program

%%

program:
  | declaration_list EOF { $1 }

declaration_list:
  | declaration { [$1] }
  | declaration SEMICOLON declaration_list { $1 :: $3 }

declaration:
  | FUNCTION IDENT LPAREN RPAREN LBRACE instruction_list RBRACE
      { DFunction ($2, [], None, $6) }

instruction_list:
  | instruction { [$1] }
  | instruction SEMICOLON instruction_list { $1 :: $3 }

instruction:
  | expression { IExpr $1 }
  | IF LPAREN expression RPAREN instruction ELSE instruction { IIf ($3, $5, Some $7) }
  | IF LPAREN expression RPAREN instruction { IIf ($3, $5, None) }
  | RETURN expression { IReturn (Some $2) }
expression:
  | INT { EConstInt $1 }
  | BOOL { EConstBool $1 }
  | IDENT { EIdent $1 }
  | expression PLUS expression { EBinOp ("+", $1, $3) }
  | LPAREN expression RPAREN { $2 }
