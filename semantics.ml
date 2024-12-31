(* semantics.ml *)
open Ast
open List

(* vérification de portée*)
exception ScopeError of string

let rec check_scope env = function
  | EIdent id -> if not (List.mem id env) then raise (ScopeError ("Undeclared variable: " ^ id))
  | EBinOp (_, e1, e2) -> check_scope env e1; check_scope env e2
  | EUnaryOp (_, e) -> check_scope env e
  | EArray es -> List.iter (check_scope env) es
  | _ -> ()

(* vérification des types*)
exception TypeError of string

let rec infer_type env = function
  | EConstInt _ -> TInt
  | EConstBool _ -> TBool
  | EBinOp ("+", e1, e2) ->
      let t1 = infer_type env e1 in
      let t2 = infer_type env e2 in
      if t1 = TInt && t2 = TInt then TInt
      else raise (TypeError "Type mismatch in addition")
  | _ -> TAny

(*Génération de JavaScript: *)
(*fonction pour convertir l'AST en JavaScript*)
let rec generate_js = function
  | EConstInt n -> string_of_int n
  | EConstBool b -> string_of_bool b
  | EBinOp (op, e1, e2) -> generate_js e1 ^ " " ^ op ^ " " ^ generate_js e2
  | _ -> ""

