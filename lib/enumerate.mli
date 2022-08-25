(** Enumerative synthesis

    This module (generically) implements the algorithm described in
    {{: https://people.csail.mit.edu/asolar/SynthesisCourse/Lecture3.htm }
    Lecture 3 of Armando Solar-Lezama's synthesis notes }.
*)

(** [search max_iterations grow prune is_correct] performs an enumerative search
    as defined in lecture notes above.

    The search is parameterized by the search space terminals ([terminals]),
    how to grow the search space ([grow]), how to prune observationally
    equivalent terms ([prune]), the notion of satisfaction ([is_correct]), and
    the maximum number of times to grow the search space [max_iterations]. *)
val search
  :  max_iterations:int
  -> terminals:'e list
  -> grow:('e list -> 'e list)
  -> prune:('e list -> 'e list)
  -> is_correct:('e -> bool)
  -> 'e option
