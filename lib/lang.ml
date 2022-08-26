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
  | EApp of exp * exp list
  | EAbs of id list * exp
  | EMatch of exp * branch list
  | ECtor of tag * exp
[@@deriving sexp, ord]

(** A library of expressions; identifiers map to pairs of parameter names and
    right-hand sides *)
type library = (id, id list * exp, String.comparator_witness) Map.t
