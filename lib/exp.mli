(** Expression utilities

    This module provides functions for working with {!val:Lang.exp}s. *)

open Core
open Lang

(** An alias for working with {!val:Lang.exp} *)
type t = Lang.exp

(** Comparator witness *)
type comparator_witness

(** Comparator  *)
val comparator : (t, comparator_witness) Base.Comparator.t

(** [show e] pretty-prints the expression [e]. *)
val show : t -> string

(** [map_branches branches ~f] applies [f] to the right-hand sides of each
    branch in [branches]. *)
val map_branches : branch list -> f:(t -> t) -> branch list

(** TODO *)
val decompose_abs : exp -> id list * exp

val decompose_app : exp -> exp * exp list
val build_abs : (id * typ) list -> exp -> exp
val build_app : exp -> exp list -> exp

(** [free_variables e] returns the free variables of [e] *)
val free_variables : t -> (id, String.comparator_witness) Set.t

(** [substitute (lhs, rhs) e] substitutes [lhs] for [rhs] in [e], alpha-renaming
    as necessary. *)
val substitute : id * t -> t -> t

(** [alpha_equivalent e1 e2] returns [true] if and only if [e1] and [e2] are
    alpha equivalent. *)
val alpha_equivalent : t -> t -> bool

(** [beta_normalize e] recursively reduces all beta redexes (pending function
    applications) in [e]. *)
val beta_normalize : t -> t
