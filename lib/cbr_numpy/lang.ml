open Core

type id = string [@@deriving sexp, compare]

type expr =
  | Num of int
  | Index of expr * expr
  | Call of expr * expr list
  | Str of string
  | Name of id
[@@deriving sexp, compare]

type lhs =
  | Name of id
  | Index of lhs * expr
[@@deriving sexp, compare]

type stmt =
  | Assign of lhs * expr
  | For of id * expr * block
  | Return of expr

and block = stmt list [@@deriving sexp, compare]

type defn = id list * block [@@deriving sexp, compare]
type env = defn String.Map.t [@@deriving sexp, compare]
type program = env * block [@@deriving sexp, compare]
