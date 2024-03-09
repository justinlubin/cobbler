(** Language module

    This module includes all the type definitions necessary to interoperate
    between all other modules. In particular, it provides the type definitions
    for all concepts related to the language definition. *)

open Core

(** Hole types *)
type hole_type =
  | Number
  | Array
  | String
  | List
  | Constant
[@@deriving compare, eq, show, hash]

(** Identifiers *)
type id = string [@@deriving sexp, compare, eq, show, hash]

(** Expressions *)
type expr =
  | Num of int
  | Index of expr * expr
  | Call of expr * expr list
  | Str of string
  | Name of id
  | Hole of hole_type * string
[@@deriving compare, eq, show]

(** Patterns *)
type pat =
  | PName of id
  | PIndex of pat * expr
  | PHole of hole_type * string
[@@deriving compare, eq, show]

(** Statements *)
type stmt =
  | Assign of pat * expr
  | For of pat * expr * block
  | Return of expr
  | If of expr * block * block
[@@deriving compare, show, eq]

(** Blocks (lists of statements) *)
and block = stmt list [@@deriving compare, show, eq]

(** Function definitions *)
type defn = id list * block [@@deriving compare]

(** Environments (binds names to functions) *)
type env = defn String.Map.t [@@deriving compare]

(** Programs (an environment and a "main" block) *)
type program = env * block [@@deriving compare, ord]

(** Hole substitutions *)
type substitutions = expr String.Map.t [@@deriving compare]

(** Expression maps *)
type exprMap = (expr, expr) Map.Poly.t
