(** Enumerative search

    This module implements a generalization of the enumerative synthesis
    algorithm described in
    {{: https://people.csail.mit.edu/asolar/SynthesisCourse/Lecture3.htm }
    Lecture 3 of Armando Solar-Lezama's synthesis notes }.
*)

open Core

(** [search ~max_iterations ~initial_space ~grow ~correct] repeatedly applies
    [grow] to [initial_space] at most [max_iterations] times searching for any
    value that satisfies the [correct] predicate. *)
val search
  :  max_iterations:int
  -> initial_space:'e list
  -> grow:('e list -> 'e list)
  -> correct:('e -> bool)
  -> 'e option
