(** Higher order unification

    This module implements Huet's 1974 higher-order pre-unification
    semi-algorithm introduced in "A Unification Algorithm for Typed
    λ-Calculus" under the assumption that the axiom of extensionality
    (eta-expansion) holds. *)
open Lang

(** The result of the unification algorithm *)
type unification_result =
  | Solved of (string * exp) list
  | Impossible
  | OutOfFuel

(** [unify fuel e0 e0'] attempts at most [fuel] iterations of Huet's 1974
    higher-order pre-unification semi-algorithm to compute a substitution
    [sigma] such that [sigma e0 == sigma e0']. *)
val unify : int -> exp -> exp -> unification_result
