open Core


type id = string [@@deriving compare, sexp]

type expr =
  | Num of int
  | Index of expr * expr
  | Call of expr * expr list
  | Str of string
  | Name of id
[@@deriving compare]

type pat =
  | Name of id
  | Index of pat * expr
[@@deriving compare]

type stmt =
  | Assign of pat * expr
  | For of pat * expr * block
  | Return of expr
[@@deriving compare]

and block = stmt list [@@deriving compare]

type defn = id list * block [@@deriving compare]
type env = defn Map.M(String).t [@@deriving compare]
type program = env * block [@@deriving compare]
