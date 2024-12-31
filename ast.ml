(* 
   ast.ml : Modélisation des structures abstraites des programmes TpScrpt en OCaml.
   Ce fichier contient les définitions des types utilisés pour représenter
   l'arbre de syntaxe abstraite (AST) des programmes écrits en TpScrpt.
*)

(* Représentation des types *)
type type_ =
  | TInt                     (* Type entier *)
  | TBool                    (* Type booléen *)
  | TString                  (* Type chaîne de caractères *)
  | TArray of type_          (* Type tableau, contenant des éléments d'un type donné *)
  | TAny                     (* Type universel, utilisé quand le type est inconnu ou générique *)
  | TObject of (string * type_) list (* Type objet, avec des champs nommés et leurs types *)
  | TUnion of type_ list     (* Union de types : permet de représenter plusieurs types possibles *)

(* Représentation des expressions *)
type expression =
  | EConstInt of int         (* Constante entière *)
  | EConstBool of bool       (* Constante booléenne *)
  | EConstString of string   (* Constante chaîne de caractères *)
  | EIdent of string         (* Identifiant : nom de variable ou autre *)
  | EBinOp of string * expression * expression 
      (* Opération binaire : opérateur (ex. "+") et deux sous-expressions *)
  | EUnaryOp of string * expression
      (* Opération unaire : opérateur (ex. "-") et une sous-expression *)
  | EArray of expression list
      (* Tableau : une liste d'expressions représentant les éléments du tableau *)
  | EObject of (string * expression) list
      (* Objet : une liste de paires (nom de champ, expression) *)
  | EAccess of expression * string
      (* Accès à un champ d'un objet ou à un élément d'un tableau *)
  | ECall of expression * expression list
      (* Appel de fonction : expression représentant la fonction et liste d'arguments *)

(* Représentation des instructions *)
type instruction =
  | IExpr of expression      (* Instruction : une simple expression, comme `x = 5;` *)
  | IVarDecl of string * type_ option * expression option
      (* Déclaration de variable : nom, type optionnel et expression d'initialisation optionnelle *)
  | IBlock of instruction list
      (* Bloc d'instructions, délimité par des accolades `{ ... }` *)
  | IIf of expression * instruction * instruction option
      (* Conditionnelle : expression de condition, branche "then" et branche "else" optionnelle *)
  | IWhile of expression * instruction
      (* Boucle while : condition et corps de la boucle *)
  | IReturn of expression option
      (* Instruction de retour : expression optionnelle (pour le cas d'un `return;`) *)

(* Représentation des déclarations *)
type declaration =
  | DTypeAlias of string * type_
      (* Alias de type : nom et type associé *)
  | DVar of string * type_ option * expression option
      (* Déclaration de variable globale : nom, type optionnel et expression d'initialisation optionnelle *)
  | DFunction of string * (string * type_ option) list * type_ option * instruction list
      (* Déclaration de fonction : nom, liste des arguments (avec leurs types optionnels),
         type de retour optionnel et corps de la fonction *)

(* Représentation d'un programme complet *)
type program = declaration list
  (* Un programme est une liste de déclarations globales *)
