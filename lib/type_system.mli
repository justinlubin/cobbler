open Lang

(** [check gamma e tau] returns [true] if and only if [e] has type [tau] in
   the environment [gamma]. *)
val check : typ_env -> exp -> typ -> bool

val infer : typ_env -> exp -> typ
