(** [dedup_by xs ~f] returns a list of representative elements of the
    equivalence classes of [xs] generated by [f]. *)
val dedup_by : 'a list -> f:('a -> 'a -> bool) -> 'a list

(** [find_and_remove_first xs ~f] tries to returns the first [x] in [xs] that
    satisfies [f], also returning the list [xs] with that element removed. *)
val find_and_remove_first : 'a list -> f:('a -> bool) -> ('a * 'a list) option

(** [gensym prefix] generates a string with the following properties:
    + Starts with two underscores
    + Does not contain any dollar signs ($)
    + Guaranteed to be unique among all strings produced by [gensym] called with
      the argument [prefix] in the course of a program's execution *)
val gensym : string -> string
