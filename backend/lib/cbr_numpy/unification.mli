(* Unification:
 * This module unifies 2 expressions with holes in them, providing a substitution for all the holes so that the 2 programs are equivalent
 *)

open Lang
open Core
open Egraph
open Ego.Generic

(* [unify_naive program1 program2] takes in 2 program and returns a list of mappings from holes in [program2] to values in [program1]. 
 *  Returns None if no substitutions can be made. Note: this assumes that [program1] contains no holes. 
 *  Will raise an error if there are two holes with the same name. Performs unification using naive pattern matching*)
val unify_naive
  :  ?debug:bool
  -> target:program
  -> pattern:program
  -> unit
  -> substitutions option

(* [unify_egraph program1 program2] takes in 2 programs and returns a list of mappings from holes in [program2] to values in [program1].
 * Returns None if no substitutions can be made. Note: this assumes that [program1] contains no holes. Performs unification using e-graphs. *)
val unify_egraph
  :  ?debug:bool
  -> graph:rw EGraph.t
  -> pattern:program
  -> unit
  -> substitutions option

val construct_egraph : ?debug:bool -> target:program -> unit -> rw EGraph.t

val unify_egraph_full
  :  ?debug:bool
  -> target:program
  -> pattern:program
  -> unit
  -> substitutions option