open Core

type id = string
type op = Add | Mul | Sub | Div | Or | And 
type expr = BinOp of op * expr * expr | Num of int | Index of id * expr | Call of id * expr list | Str of string | Name of id
type stmt = Assign of expr * expr | For of expr * expr * block | Return of expr
and block = stmt list
type env = (id * id list, block, String.comparator_witness) Map.t
