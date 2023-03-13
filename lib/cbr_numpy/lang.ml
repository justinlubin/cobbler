open Core

type id = string [@@deriving sexp, compare]

type expr =
  | Num of int
  | Index of expr * expr
  | Call of expr * expr list
  | Str of string
  | Name of id
[@@deriving compare]

type lhs =
  | Name of id
  | Index of lhs * expr
[@@deriving compare]

type stmt =
  | Assign of lhs * expr
  | For of id * expr * block
  | Return of expr
[@@deriving compare]

and block = stmt list [@@deriving compare]

type defn = id list * block [@@deriving compare]
type env = defn String.Map.t [@@deriving compare]
type program = env * block [@@deriving compare]
