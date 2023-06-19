(** Enumerative search

    This module provides functions to enumeratively search a space.
*)

open Core

(** [search ~max_iterations ~initial_space ~grow ~correct] repeatedly applies
    [grow] to [initial_candidates] at most [max_iterations] times searching for
    any value that satisfies the [correct] predicate. For more information, see
    {{: https://people.csail.mit.edu/asolar/SynthesisCourse/Lecture3.htm }
    Lecture 3 of Armando Solar-Lezama's synthesis notes }. *)
val bottom_up
  :  max_iterations:int
  -> initial_candidates:'e list
  -> grow:('e list -> 'e list)
  -> correct:('e -> bool)
  -> 'e option

(** [top_down ~max_iterations ~start ~expand ~correct] repeatedly applies
    [expand] to [start] at most [max_iterations] times searching for any
    value that satisfies the [correct] predicate. The first argument of
    [expand] is the current iteration depth, and the output of [correct]
    maps candidate programs to final programs. *)
val top_down
  :  max_iterations:int
  -> start:'e list
  -> expand:(int -> 'e -> 'e list)
  -> correct:('e -> 'e option)
  -> 'e option
