(*Programme Principal*)
open Ast
open Semantics
open Parser
open Lexer

let () =
  let lexbuf = Lexing.from_channel stdin in
  try
    let ast = Parser.program Lexer.tokenize lexbuf in
    check_scope [] ast;
    Printf.printf "Parsed and checked successfully!\n"
  with
  | Lexer.Error msg -> Printf.eprintf "Lexer error: %s\n" msg
  | Parser.Error -> Printf.eprintf "Syntax error\n"
  | ScopeError msg -> Printf.eprintf "Scope error: %s\n" msg
  | TypeError msg -> Printf.eprintf "Type error: %s\n" msg