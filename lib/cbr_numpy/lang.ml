open Core

type hole_type =
  | Number
  | Array
[@@deriving compare, eq, show]

type id = string [@@deriving sexp, compare, eq, show, hash]

type expr =
  | Num of int
  | Index of expr * expr
  | Call of expr * expr list
  | Str of string
  | Name of id
  | Hole of hole_type * string
[@@deriving compare, eq, show]

type pat =
  | PName of id
  | PIndex of pat * expr
  | PHole of hole_type * string
[@@deriving compare, eq, show]

type stmt =
  | Assign of pat * expr
  | For of pat * expr * block
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
[@@deriving ord, hash]

type stmtType =
  | SFor
  | SAssign
  | SReturn
[@@deriving ord, hash]

type patType =
  | LName of string
  | LIndex
  | LHole
[@@deriving ord, hash]
