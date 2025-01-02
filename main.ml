open Ast
open Lexing
open Parser
open Semantics

(* Pour le débogage *)
let debug msg = Printf.fprintf stderr "[DEBUG] %s\n%!" msg

(* Gestion des erreurs de parsing *)
let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "Line: %d, Position: %d"
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

let parse_with_error lexbuf =
  try
    debug "Starting parsing...";
    let result = Parser.program Lexer.tokenize lexbuf in
    debug "Parsing completed successfully";
    result
  with
  | Parsing.Parse_error ->
      let pos = print_position lexbuf in
      failwith (Printf.sprintf "Parse error at %s" pos)
  | Failure msg ->
      let pos = print_position lexbuf in
      failwith (Printf.sprintf "Lexing error at %s: %s" pos msg)

(* Programme principal *)
let () =
  try
    (* Lecture du fichier source *)
    let input_file = Sys.argv.(1) in
    debug (Printf.sprintf "Processing file: %s" input_file);
    
    (* Ouverture et lecture du fichier *)
    let ic = open_in input_file in
    let lexbuf = Lexing.from_channel ic in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = input_file };
    
    (* Parsing *)
    debug "Starting parsing phase";
    let ast = parse_with_error lexbuf in
    debug (Printf.sprintf "AST contains %d declarations" (List.length ast));
    
    (* Vérification de la portée *)
    debug "Starting scope checking";
    check_scope_program ast;
    debug "Scope checking completed";
    
    (* Vérification des types *)
    debug "Starting type checking";
    type_check_program ast;
    debug "Type checking completed";
    
    (* Fermeture du fichier *)
    close_in ic;
    
    (* Message de succès *)
    Printf.printf "Compilation successful!\n"
    
  with
  | Sys_error msg ->
      Printf.eprintf "System error: %s\n" msg;
      exit 1
  | ScopeError msg ->
      Printf.eprintf "Scope error: %s\n" msg;
      exit 1
  | TypeError msg ->
      Printf.eprintf "Type error: %s\n" msg;
      exit 1
  | e ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e);
      exit 1