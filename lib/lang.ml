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
[@@deriving sexp, ord, eq, compare]

(** An environment of expressions *)
type env = (id, exp, String.comparator_witness) Map.t

(** Useful as a comparator module *)
module Exp = struct
  module T = struct
    type t = exp

    let compare = compare_exp
    let sexp_of_t = sexp_of_exp
  end

  include T
  include Comparator.Make (T)
end
