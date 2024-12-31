(*Identification les différents tokens du langage TpScrpt*)

{
  open Parser
  exception Error of string
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let whitespace = [' ' '\t' '\n']
let ident = letter (letter | digit | '_')*

rule tokenize = parse
  | whitespace+ { tokenize lexbuf }
  | "//" [^ '\n']* { tokenize lexbuf }  (* Commentaire ligne *)
  | "/*" [^ '*']* "*/" { tokenize lexbuf }  (* Commentaire bloc *)
  | "true" { BOOL true }
  | "false" { BOOL false }
  | digit+ as n { INT (int_of_string n) }
  | ident as id { IDENT id }
  | "=" { EQ }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | "function" { FUNCTION }
  | "return" { RETURN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "if" { IF }
  | "else" { ELSE }
  | ";" { SEMICOLON }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | eof { EOF }
  | _ { raise (Error "Invalid token") }
