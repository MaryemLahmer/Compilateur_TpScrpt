type type_ =
  | TInt
  | TBool
  | TString
  | TArray of type_
  | TAny
  | TObject of (string * type_) list
  | TUnion of type_ list

type expression =
  | EConstInt of int
  | EConstBool of bool
  | EConstString of string
  | EIdent of string
  | EBinOp of string * expression * expression
  | EUnaryOp of string * expression
  | EArray of expression list
  | EObject of (string * expression) list
  | EAccess of expression * string
  | ECall of expression * expression list

type instruction =
  | IExpr of expression
  | IVarDecl of string * type_ option * expression option
  | IBlock of instruction list
  | IIf of expression * instruction * instruction option
  | IWhile of expression * instruction
  | IReturn of expression option

type declaration =
  | DTypeAlias of string * type_
  | DVar of string * type_ option * expression option
  | DFunction of string * (string * type_ option) list * type_ option * instruction list

type program = declaration list