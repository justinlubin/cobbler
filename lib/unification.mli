(** Higher order unification

    This module implements Huet's 1974 higher-order pre-unification
    semi-algorithm. *)

open Core
open Lang

(** The result of the unification algorithm *)
type unification_result =
  | Solved of (id * exp) list
  | Impossible
  | OutOfFuel

(** [unify fuel e0 e0'] attempts at most [fuel] iterations of Huet's 1974
    higher-order pre-unification semi-algorithm to compute a substitution
    [sigma] such that [sigma e0 == sigma e1]. *)
val unify : int -> exp -> exp -> unification_result
