(** Higher order unification

    This module implements Huet's 1974 higher-order pre-unification
    semi-algorithm introduced in "A Unification Algorithm for Typed
    λ-Calculus." *)

open Core
open Lang

(** Atoms in the language supported for unification *)
type atom =
  | Variable of string
  | Constant of string

(** Terms in the language supported for unification *)
type term =
  | Atom of atom
  | Application of term * term
  | Abstraction of string * term

(** The result of the unification algorithm *)
type unification_result =
  | Solved of (id * term) list
  | Impossible
  | OutOfFuel

(** [unify fuel e0 e0'] attempts at most [fuel] iterations of Huet's 1974
    higher-order pre-unification semi-algorithm to compute a substitution
    [sigma] such that [sigma e0 == sigma e0']. *)
val unify : int -> term -> term -> unification_result
