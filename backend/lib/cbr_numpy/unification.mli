(** Unification

    This module provides functions to unify two programs with holes in them
    (sketches), providing a substitution for all the holes so that the programs
    are equivalent.

    The [unify_naive] function is syntactic unification, and the
    [unify_egraph_full] function uses e-graphs to perform restricted semantic
    unification. *)

open Lang
open Core
open Egraph
open Ego.Generic

(* [unify_naive program1 program2] takes in two program and returns a list of
   mappings from holes in [program2] to values in [program1] using syntactic
   unification (naive pattern matching), returning [None] if no such
   substitution can be found.

   **Note**: this function (i) assumes that [program1] contains no holes, and
   (ii) will raise an exception if there are two holes with the same name. *)
val unify_naive
  :  ?debug:bool
  -> target:program
  -> pattern:program
  -> unit
  -> substitutions option

(** [construct_egraph ~target ()] constructs an e-graph from the program
    [target]. *)
val construct_egraph : ?debug:bool -> target:program -> unit -> rw EGraph.t

(* [unify_egraph_full program1 program2] takes in two program and returns a
   list of mappings from holes in [program2] to values in [program1] using
   e-graph unification, returning [None] if no such substitution can be found.

   **Note**: this function (i) assumes that [program1] contains no holes, and
   (ii) will raise an exception if there are two holes with the same name. *)
val unify_egraph_full
  :  ?debug:bool
  -> target:program
  -> pattern:program
  -> unit
  -> substitutions option

(** [unify_egraph] does the same thing as [unify_egraph_full] but operates
    on one e-graph and one program. *)
val unify_egraph
  :  ?debug:bool
  -> graph:rw EGraph.t
  -> pattern:program
  -> unit
  -> substitutions option
