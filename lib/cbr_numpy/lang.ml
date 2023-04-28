open Core

type hole_type =
  | Number
  | Array
[@@deriving compare, eq]

type id = string [@@deriving sexp, compare, eq]

type expr =
  | Num of int
  | Index of expr * expr
  | Call of expr * expr list
  | Str of string
  | Name of id
  | Hole of hole_type * string
[@@deriving compare, eq]

type pat =
  | PName of id
  | PIndex of pat * expr
  | PHole of hole_type * string
[@@deriving compare, eq]

type stmt =
  | Assign of pat * expr
  | For of pat * expr * block
  | Return of expr
[@@deriving compare]

and block = stmt list [@@deriving compare]

type defn = id list * block [@@deriving compare]
type env = defn String.Map.t [@@deriving compare]
type program = env * block [@@deriving compare]
type substitutions = expr String.Map.t [@@deriving compare]
