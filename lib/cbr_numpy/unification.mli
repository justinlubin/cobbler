(* Unification:
 * This module unifies 2 expressions with holes in them, providing a substitution for all the holes so that the 2 programs are equivalent
 *)

open Lang
open Core

(* [unify program1 program2] takes in 2 program and returns a list of mappings from holes in [program2] to values in [program1]. 
 *  Returns None if no substitutions can be made. Note: this assumes that [program1] contains no holes. *)
val unify : program -> program -> substitutions option
