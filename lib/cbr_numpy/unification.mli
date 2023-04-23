(* Unification:
 * This module unifies 2 expressions with holes in them, providing a substitution for all the holes so that the 2 programs are equivalent
 *)

open Lang
open Core

(* [unify_naive program1 program2] takes in 2 program and returns a list of mappings from holes in [program2] to values in [program1]. 
 *  Returns None if no substitutions can be made. Note: this assumes that [program1] contains no holes. 
 *  Will raise an error if there are two holes with the same name. Performs unification using naive pattern matching*)
val unify_naive : target:program -> pattern:program -> substitutions option

(* [unify_egraph program1 program2] takes in 2 programs and returns a list of mappings from holes in [program2] to values in [program1].
 * Returns None if no substitutions can be made. Note: this assumes that [program1] contains no holes. Performs unification using e-graphs. *)
val unify_egraph : target:program -> pattern:program -> substitutions option
