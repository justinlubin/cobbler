(** Language module.

    This module includes all the type definitions necessary to interoperate
    between all other modules. In particular, it provides the type definitions
    for all concepts related to the language definition. *)

open Base

(** Expression identifiers *)
type id = string

(** Constructor tags (names) *)
and tag = string

(** Case branches *)
and branch = tag * (id * exp)

(** Expressions *)
and exp =
  | EVar of id
  | EApp of exp * exp
  | EAbs of id * exp
  | EMatch of exp * branch list
  | ECtor of tag * exp
  | EInt of int
[@@deriving sexp, ord, eq]

(** An environment of expressions *)
type env = (id, exp, String.comparator_witness) Map.t
