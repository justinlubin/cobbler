open Core

type id = string [@@deriving sexp, compare, eq, show]

type expr =
  | Num of int
  | Index of expr * expr
  | Call of expr * expr list
  | Str of string
  | Name of id
  | Hole of string
[@@deriving compare, eq, show]

type lhs =
  | Name of id
  | Index of lhs * expr
[@@deriving compare, eq, show]

type stmt =
  | Assign of lhs * expr
  | For of id * expr * block
  | Return of expr
[@@deriving compare, show]

and block = stmt list [@@deriving compare, show]

type defn = id list * block [@@deriving compare]
type env = defn String.Map.t [@@deriving compare]
type program = env * block [@@deriving compare, ord]
type substitutions = expr String.Map.t [@@deriving compare]

type exprType =
  | ENum of int
  | EIndex
  | ECall
  | EStr of string
  | EName of string
  | EHole
[@@deriving ord]

type stmtType =
  | SFor
  | SAssign
  | SReturn
[@@deriving ord]

type lhsType =
  | LName of string
  | LIndex
[@@deriving ord]
