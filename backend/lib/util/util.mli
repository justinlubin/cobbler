(** Common utilities *)

(** [dedup_by xs ~f] returns a list of representative elements of the
    equivalence classes of [xs] generated by [f]. *)
val dedup_by : 'a list -> f:('a -> 'a -> bool) -> 'a list

(** [find_and_remove_first xs ~f] tries to returns the first [x] in [xs] that
    satisfies [f], also returning the list [xs] with that element removed. *)
val find_and_remove_first : 'a list -> f:('a -> bool) -> ('a * 'a list) option

(** [gensym prefix] generates the following string:
      __prefix#<unique string>
    where <unique string> is guaranteed to:
      + be unique among all calls to [gensym] in a program
      + not contain any dollar signs ($) *)
val gensym : string -> string

(** [ungensym (gensym prefix)] returns [prefix]. *)
val ungensym : string -> string

(** [embed_name prefix metadata] encodes prefix and metadata in a gensymmed
    string using a dollar sign ($). *)
val embed_name : string -> string -> string

(** [unembed_name (embed_name prefix metadata)] returns
    [Some (prefix, metadata)]. *)
val unembed_name : string -> (string * string) option
