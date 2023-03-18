open Core

type id = string [@@deriving sexp, compare, eq]

type expr =
  | Num of int
  | Index of expr * expr
  | Call of expr * expr list
  | Str of string
  | Name of id
  | Hole of string
[@@deriving compare, eq]

type lhs =
  | Name of id
  | Index of lhs * expr
[@@deriving compare, eq]

type stmt =
  | Assign of lhs * expr
  | For of id * expr * block
  | Return of expr
[@@deriving compare]

and block = stmt list [@@deriving compare]

type defn = id list * block [@@deriving compare]
type env = defn String.Map.t [@@deriving compare]
type program = env * block [@@deriving compare]
type substitutions = expr String.Map.t [@@deriving compare]
