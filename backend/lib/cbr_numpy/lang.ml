open Core

type hole_type =
  | Number
  | Array
[@@deriving compare, eq, show, hash]

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
  | If of expr * block * block
[@@deriving compare, show, eq]

and block = stmt list [@@deriving compare, show, eq]

type defn = id list * block [@@deriving compare]
type env = defn String.Map.t [@@deriving compare]
type program = env * block [@@deriving compare, ord]
type substitutions = expr String.Map.t [@@deriving compare]
type exprMap = (expr, expr) Map.Poly.t
