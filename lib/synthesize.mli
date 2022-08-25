open Lang

(** [synthesize lib e] tries to find an expression [e'] that is
    + semantically equivalent to [e], and
    + defined solely in terms of compositions of combinators from [lib] (and
      constants). *)
val synthesize : library -> exp -> exp option
