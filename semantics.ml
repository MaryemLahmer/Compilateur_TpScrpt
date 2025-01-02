open Ast

(* Exceptions *)
exception ScopeError of string
exception TypeError of string

(* Environment type *)
type env = {
  variables: (string * type_ option) list;
  functions: (string * ((string * type_ option) list * type_ option)) list;
}

(* Create an empty environment *)
let empty_env = {
  variables = [];
  functions = [];
}

(* Add variable to environment *)
let add_variable env name typ =
  { env with variables = (name, typ) :: env.variables }

(* Add function to environment *)
let add_function env name params ret_type =
  { env with functions = (name, (params, ret_type)) :: env.functions }

(* Check scope for expressions *)
let rec check_scope_expression env = function
  | EConstInt _ | EConstBool _ | EConstString _ -> ()
  | EIdent name ->
      if not (List.exists (fun (var_name, _) -> var_name = name) env.variables) then
        raise (ScopeError ("Undefined variable: " ^ name))
  | EBinOp (_, e1, e2) ->
      check_scope_expression env e1;
      check_scope_expression env e2
  | EUnaryOp (_, e) ->
      check_scope_expression env e

(* Check scope for instructions *)
let rec check_scope_instruction env = function
  | IExpr e ->
      check_scope_expression env e;
      env
  | IReturn expr_opt ->
      (match expr_opt with
       | Some expr -> check_scope_expression env expr
       | None -> ());
      env
  | IBlock instrs ->
      List.fold_left (fun acc instr -> check_scope_instruction acc instr) env instrs
  | IIf (cond, then_branch, else_opt) ->
      check_scope_expression env cond;
      let env = check_scope_instruction env then_branch in
      (match else_opt with
       | Some else_branch -> check_scope_instruction env else_branch
       | None -> env)
  | IWhile (cond, body) ->
      check_scope_expression env cond;
      check_scope_instruction env body

(* Check scope for declarations *)
let check_scope_declaration env = function
  | DFunction (name, params, ret_type, body) ->
      let env = add_function env name params ret_type in
      let env = 
        List.fold_left
          (fun acc (param_name, param_type) -> add_variable acc param_name param_type)
          env
          params
      in
      List.fold_left (fun acc instr -> check_scope_instruction acc instr) env body

(* Infer the type of an expression *)
let rec infer_type env = function
  | EConstInt _ -> TInt
  | EConstBool _ -> TBool
  | EConstString _ -> TString
  | EIdent name ->
      (try 
         match List.assoc name env.variables with
         | Some t -> t
         | None -> TAny
       with Not_found -> raise (TypeError ("Undefined variable: " ^ name)))
  | EBinOp (op, e1, e2) ->
      let t1 = infer_type env e1 in
      let t2 = infer_type env e2 in
      (match op, t1, t2 with
       | ("+"|"-"|"*"|"/"), TInt, TInt -> TInt
       | _ -> raise (TypeError "Invalid operator for types"))
  | EUnaryOp ("-", e) ->
      let t = infer_type env e in
      if t = TInt then TInt
      else raise (TypeError "Unary minus requires integer")
  | EUnaryOp (_, _) ->
      raise (TypeError "Unknown unary operator")

(* Type check a program *)
let type_check_program program =
  let env = empty_env in
  List.iter (fun decl ->
    match decl with
    | DFunction (name, params, ret_type, body) ->
        let env = add_function env name params ret_type in
        List.iter (fun instr ->
          match instr with
          | IReturn (Some e) ->
              let t = infer_type env e in
              (match ret_type with
               | Some expected when t <> expected ->
                   raise (TypeError "Return type mismatch")
               | _ -> ())
          | _ -> ()) body) program

(* Check scope for the entire program *)
let check_scope_program program =
  ignore (List.fold_left check_scope_declaration empty_env program)