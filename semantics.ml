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


  (* Vérification de portée pour un programme complet *)
let rec check_scope_program env (program : Ast.program) =
  List.iter (fun decl ->
    match decl with
    | DFunction (name, args, _, body) ->
        (* Ajoutez les arguments de la fonction à l'environnement *)
        let env_with_args = List.fold_left (fun acc (arg_name, _) -> arg_name :: acc) env args in
        (* Vérifiez la portée dans le corps de la fonction *)
        List.iter (check_scope env_with_args) body
    | DVar (name, _, Some expr) ->
        (* Vérifiez l'expression d'initialisation *)
        check_scope env expr
    | DVar (_, _, None) ->
        (* Pas d'expression d'initialisation, rien à vérifier *)
        ()
    | _ -> ()
  ) program
  