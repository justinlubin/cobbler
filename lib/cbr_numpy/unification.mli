(* Unification:
 * This module unifies 2 expressions with holes in them, providing a substitution for all the holes so that the 2 programs are equivalent
 *)

open Lang
open Core

(* [unify] takes in 2 expressions and a returns a substitution of holes into values*)
val unify : expr -> expr -> substitution
