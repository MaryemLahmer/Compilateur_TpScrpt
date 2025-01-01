{
  open Parser
  exception Error of string
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let whitespace = [' ' '\t' '\n']
let ident = letter (letter | digit | '_')*
let string = '"' [^ '"']* '"'

rule tokenize = parse
  | whitespace+ { tokenize lexbuf }
  | "//" [^ '\n']* '\n' { tokenize lexbuf }  (* Commentaire ligne *)
  | "/*" ([^ '*'] | ('*' [^ '/']))* "*/" { tokenize lexbuf }  (* Commentaire bloc *)
  | string as s { STRING (String.sub s 1 (String.length s - 2)) }
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
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "if" { IF }
  | "else" { ELSE }
  | ";" { SEMICOLON }
  | eof { EOF }
  | _ { raise (Error (Printf.sprintf "Invalid character: %s" (Lexing.lexeme lexbuf))) }