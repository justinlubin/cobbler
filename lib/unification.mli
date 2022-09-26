(** Higher order unification

    This module implements Huet's 1974 higher-order pre-unification
    semi-algorithm introduced in "A Unification Algorithm for Typed
    λ-Calculus." 
    
    This module relies on the following assumptions:
    - Atoms of distinct types have distinct variable names
    - Eta-expansion is allowed *)

open Core

(** Types in the language supported for unification *)
type typ =
  | Elementary of Lang.typ
  | Arrow of typ * typ

(** Atoms in the language supported for unification *)
type atom =
  | Variable of string * typ
  | Constant of string * typ

(** Terms in the language supported for unification *)
type term =
  | Atom of atom
  | Application of term * term
  | Abstraction of string * typ * term

(** [typ t] returns the type of [t]. *)
val typ : term -> typ

(** Assuming [t] is in normal form, [abbreviate t] returns the canonical
    decomposition of [t] into its header, its head, and its arguments. This is
    a useful helper function for dealing with {!val:term}s. *)
val abbreviate : term -> (string * typ) list * atom * term list

(** The result of the unification algorithm *)
type unification_result =
  | Solved of (string * term) list
  | Impossible
  | OutOfFuel

(** [unify fuel e0 e0'] attempts at most [fuel] iterations of Huet's 1974
    higher-order pre-unification semi-algorithm to compute a substitution
    [sigma] such that [sigma e0 == sigma e0']. *)
val unify : int -> term -> term -> unification_result
