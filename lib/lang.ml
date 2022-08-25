(** Language module.

    This module includes all the type definitions necessary to interoperate
    between all other modules. In particular, it provides the type definitions
    for all concepts related to the language definition. *)

open Base

(* TODO: need to expand this definition *)

(** Patterns *)
type pat = PVar of string

(* TODO: need to expand this definition *)

(** Expressions *)
type exp =
  | EVar of string
  | EApp of exp * exp

(** A library of expressions *)
type library = (string, exp, String.comparator_witness) Map.t
