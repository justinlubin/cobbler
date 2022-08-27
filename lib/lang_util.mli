(** Language utilities.

    This module provides helper functions specifically for working with the
    types defined in {!module:Lang}. *)

open Lang

(** [show_exp e] pretty-prints the expression [e]. *)
val show_exp : exp -> string

(** [map_branches branches ~f] applies [f] to the right-hand sides of each
    branch in [branches]. *)
val map_branches : branch list -> f:(exp -> exp) -> branch list

(** [substitute (lhs, rhs) e] substitutes [lhs] for [rhs] in [e], alpha-renaming
    as necessary. *)
val substitute : id * exp -> exp -> exp

(** [alpha_equivalent e1 e2] returns [true] if and only if [e1] and [e2] are
    alpha equivalent. *)
val alpha_equivalent : exp -> exp -> bool
