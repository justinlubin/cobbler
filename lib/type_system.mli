open Lang

exception IllTyped of exp

(** [infer gamma e] returns [tau] if and only if [e] has type [tau] in
    the datatype environment [sigma] and type environment [gamma] and throws the
    exception {!val:IllTyped} otherwise. *)
val infer : datatype_env -> typ_env -> exp -> typ

(** [check gamma e tau] returns [true] if and only if [e] has type [tau] in
    the datatype environment [sigma] and type environment [gamma]. *)
val check : datatype_env -> typ_env -> exp -> typ -> bool

(** [well_typed sigma gamma env] returns [true] if and only if [env] is
    well-typed in the datatype environment [sigma] and type environment
    [gamma]. *)
val well_typed : datatype_env -> typ_env -> env -> bool
