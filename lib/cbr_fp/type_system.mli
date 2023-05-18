open Lang

exception IllTyped of exp

(** [infer gamma e] returns [tau] if and only if [e] has type [tau] in
    the datatype environment [sigma] and type environment [gamma] and throws the
    exception {!val:IllTyped} otherwise. *)
val infer : datatype_env -> typ_env -> exp -> typ

(** [check gamma e tau] returns [()] if and only if [e] has type [tau] in
    the datatype environment [sigma] and type environment [gamma] and throws the
    exception {!val:IllTyped} otherwise. *)
val check : datatype_env -> typ_env -> exp -> typ -> unit

(** [well_typed (sigma, gamma, env)] returns [()] if and only if [env] is
    well-typed in the datatype environment [sigma] and type environment
    [gamma] and throws the exception {!val:IllTyped} otherwise. *)
val well_typed : datatype_env * typ_env * env -> unit

(** [ctor_typ sigma tag] looks up the type of the constructor [tag] in
    [sigma], returning its datatype (with a list of parameter names) and
    its argument type list. *)
val ctor_typ
  :  datatype_env
  -> string
  -> ((string * string list) * typ list) option
