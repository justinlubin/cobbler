(** [dedup_by xs ~f ] returns a list of representative elements of the
    equivalence classes of [xs] generated by [f]. *)
val dedup_by : 'a list -> f:('a -> 'a -> bool) -> 'a list
