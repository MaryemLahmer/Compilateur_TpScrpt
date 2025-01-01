open Ast
open Lexing
open Semantics

let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "Line %d, character %d"
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "stdin" };
    let ast = Parser.program Lexer.tokenize lexbuf in
    try
      check_scope_program ast;
      Printf.printf "Program type-checked successfully!\n"
    with
    | ScopeError msg ->
        Printf.eprintf "Scope error: %s\n" msg;
        exit 1
    | TypeError msg ->
        Printf.eprintf "Type error: %s\n" msg;
        exit 1
  with
  | Lexer.Error msg ->
      Printf.eprintf "Lexical error at %s: %s\n" (print_position lexbuf) msg;
      exit 1
  | Parser.SyntaxError msg ->
      Printf.eprintf "Syntax error at %s\n" (print_position lexbuf);
      exit 1
    