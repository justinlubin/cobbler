open Lang

(* TODO *)
val check : typ_env -> exp -> typ -> bool
val infer : typ_env -> exp -> typ
