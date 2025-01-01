open Ast

(* Exceptions *)
exception ScopeError of string
exception TypeError of string

(* Environment type *)
type env = {
  variables: (string * type_ option) list;
  functions: (string * ((string * type_ option) list * type_ option)) list;
}

(* Main functions exposed by the module *)
val empty_env : env
val check_scope_expression : env -> expression -> unit
val check_scope_instruction : env -> instruction -> env
val check_scope_declaration : env -> declaration -> env
val check_scope_program : program -> unit
val type_check_program : program -> unit
val infer_type : env -> expression -> type_