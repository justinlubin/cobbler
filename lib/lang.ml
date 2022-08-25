(** Language module.

    This module includes all the type definitions necessary to interoperate
    between all other modules. In particular, it provides the type definitions
    for all concepts related to the language definition. *)

open Base

type branch = string * (string * exp)

(** Expressions *)
and exp =
  | EVar of string
  | EApp of exp * exp
  | EAbs of string * exp
  | EMatch of exp * branch list
  | ECtor of string * exp
[@@deriving sexp, ord]

(** A environment of expressions *)
type env = (string, exp, String.comparator_witness) Map.t
