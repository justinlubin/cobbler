open Lang

(** [synthesize env e] tries to find an expression [e'] that is
    + semantically equivalent to [e], and
    + defined in terms expressions from the environment [env]. *)
val synthesize : env -> exp -> exp option

val synthesize' : typ_env -> env -> (id * typ) list -> exp -> typ -> exp option
