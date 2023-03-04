open Core

type id = string

type op =
  | Add
  | Mul
  | Sub
  | Div
  | Or
  | And

type expr =
  | Num of int
  | Index of expr * expr
  | Call of expr * expr list
  | Str of string
  | Name of id

type stmt =
  | Assign of id * expr
  | For of id * expr * block
  | Return of expr

and block = stmt list

type env = (id * id list, block, String.comparator_witness) Map.t
type ast = env list
