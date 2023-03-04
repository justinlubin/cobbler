open Core

type id = string

type expr =
  | Num of int
  | Index of expr * expr
  | Call of expr * expr list
  | Str of string
  | Name of id

type lhs =
  | Name of id
  | Index of lhs * expr

type stmt =
  | Assign of lhs * expr
  | For of id * expr * block
  | Return of expr

and block = stmt list

type defn = id list * block
type env = (id, defn, String.comparator_witness) Map.t
type program = env * block
