(* semantics.ml *)
open Ast
open List

(* Vérification de portée *)
exception ScopeError of string

let rec check_scope_expression env = function
| EConstInt _ | EConstBool _ | EConstString _ | EIdent _ -> ()
| EBinOp (_, e1, e2) ->
    (* Vérifie les sous-expressions dans une opération binaire *)
    check_scope_expression env e1;
    check_scope_expression env e2
| EUnaryOp (_, e) ->
    (* Vérifie l'expression dans une opération unaire *)
    check_scope_expression env e
| EArray exprs ->
    (* Vérifie chaque élément dans un tableau *)
    List.iter (check_scope_expression env) exprs
| EObject fields ->
    (* Vérifie chaque champ dans un objet *)
    List.iter (fun (_, e) -> check_scope_expression env e) fields
| EAccess (e, _) -> check_scope_expression env e
| ECall (e, args) ->
    (* Vérifie l'expression de fonction et les arguments *)
    check_scope_expression env e;
    List.iter (check_scope_expression env) args

and check_scope_instruction env = function
| IExpr e ->
    (* Vérifie l'expression contenue dans l'instruction IExpr *)
    check_scope_expression env e;
    env
| IVarDecl (name, _, Some expr) ->
    (* Vérifie l'expression d'initialisation et ajoute la variable à l'environnement *)
    check_scope_expression env expr;
    name :: env
| IVarDecl (name, _, None) ->
    (* Ajoute la variable à l'environnement sans initialisation *)
    name :: env
| IBlock instructions ->
    (* Vérifie les instructions dans un bloc *)
    let final_env = List.fold_left 
      (fun acc instr -> check_scope_instruction acc instr) 
      env 
      instructions
    in final_env
| IIf (cond, then_branch, Some else_branch) ->
    (* Vérifie la condition et les branches then/else *)
    check_scope_expression env cond;
    let env_after_then = check_scope_instruction env then_branch in
    check_scope_instruction env_after_then else_branch
| IIf (cond, then_branch, None) ->
    (* Vérifie la condition et la branche then uniquement *)
    check_scope_expression env cond;
    check_scope_instruction env then_branch
| IWhile (cond, body) ->
    (* Vérifie la condition et le corps de la boucle *)
    check_scope_expression env cond;
    check_scope_instruction env body
| IReturn (Some expr) ->
    (* Vérifie l'expression de retour *)
    check_scope_expression env expr;
    env
| IReturn None ->
    (* Retour sans expression, pas de vérification nécessaire *)
    env

(* Vérification des types *)
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

(* Génération de JavaScript *)
let rec generate_js = function
| EConstInt n -> string_of_int n
| EConstBool b -> string_of_bool b
| EBinOp (op, e1, e2) -> generate_js e1 ^ " " ^ op ^ " " ^ generate_js e2
| _ -> ""

(* Vérification de portée pour un programme complet *)
let check_scope_program env (program : Ast.program) =
  List.iter (fun decl ->
    match decl with
    | DFunction (name, args, _, body) ->
        (* Ajouter les arguments au nouvel environnement *)
        let env_with_args = 
          List.fold_left (fun acc (arg_name, _) -> arg_name :: acc) env args 
        in
        (* Vérifier la portée dans le corps de la fonction *)
        ignore (List.fold_left 
          (fun acc instr -> check_scope_instruction acc instr) 
          env_with_args 
          body)
    | DVar (name, _, Some expr) ->
        (* Vérifier l'expression d'initialisation *)
        check_scope_expression env expr
    | DVar (_, _, None) ->
        (* Pas d'expression d'initialisation, rien à vérifier *)
        ()
    | _ -> ()
  ) program