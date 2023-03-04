open Core

type id = string

type pat =
  | Name of id
  | Index of pat * expr

type expr =
  | Num of int
  | Index of expr * expr
  | Call of expr * expr list
  | Str of string
  | Name of id

type stmt =
  | Assign of pat * expr
  | For of pat * expr * block
  | Return of expr

and block = stmt list

type env = (id, id list * block, String.comparator_witness) Map.t
type program = env * block
