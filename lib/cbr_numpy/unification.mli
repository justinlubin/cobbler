(* Unification:
 * This module unifies 2 expressions with holes in them, providing a substitution for all the holes so that the 2 programs are equivalent
 *)

open Lang
open Core

(** [unify ~target ~pattern] takes in 2 program and returns a list of mappings from holes in [~pattern] to values in [~target]. 
    Returns None if no substitutions can be made. Note: this assumes that [~target] contains no holes. 
    Will raise an error if there are two holes with the same name. 
*)
val unify : target:program -> pattern:program -> substitutions option
