(** Enumerative search

    This module implements a generalization of the enumerative synthesis
    algorithm described in
    {{: https://people.csail.mit.edu/asolar/SynthesisCourse/Lecture3.htm }
    Lecture 3 of Armando Solar-Lezama's synthesis notes }.
*)

open Core

(** The result of the [grow] function used in {!val:search}. The
    [additions] is for new (unchecked) candidates and the [new_space] field
    is the updated space. *)
type ('e, 'comp) grow_result =
  { additions : ('e, 'comp) Set.t
  ; new_space : ('e, 'comp) Set.t
  }

(** [search ~max_iterations ~initial_space ~grow ~correct] repeatedly applies
    [grow] to [initial_space] at most [max_iterations] times searching for any
    value that satisfies the [correct] predicate. *)
val search
  :  max_iterations:int
  -> initial_space:('e, 'comp) Set.t
  -> grow:(('e, 'comp) Set.t -> ('e, 'comp) grow_result)
  -> correct:('e -> bool)
  -> 'e option
