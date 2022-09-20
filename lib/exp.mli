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

(** [decompose_abs e] strips the top-level lambda abstractions from [e], so that
    if [e] is [lambda x1 : tau1 ... lambda xN : tauN . body], then
    [decompose_abs e] is [([(x1, tau1) ; ... ; (xN, tauN) ], body)]. In other
    words, [decompose_abs] "undoes" {!val:build_abs}. *)
val decompose_abs : exp -> (id * typ) list * exp

(** [decompose_app e] strips the top-level applications from [e], so that
    if [e] is [ (((head arg1) arg2) ... argN) ], then
    [decompose_app e] is [(head, [arg1 ; arg2 ; ... ; argN])]. In other words,
    [decompose_app] "undoes" {!val:build_app}. *)
val decompose_app : exp -> exp * exp list

(** [build_abs [(x1, tau1) ; ... ; (xN, tauN) ] body] returns the expression
    [lambda x1 : tau1 ... lambda xN : tauN . body]. In other words, [build_abs]
    "undoes" {!val:decompose_abs}. *)
val build_abs : (id * typ) list -> exp -> exp

(** [build_app head [arg1 ; arg2 ; ... ; argN]] returns the expression
     [ (((head arg1) arg2) ... argN) ]. In other words, [build_app] "undoes"
     {!val:decompose_app}. *)
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
