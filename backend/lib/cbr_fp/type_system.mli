(** Type system

    This module provides functions inferring and checking types
    ({!val:Lang.typ}) of expressions ({!val:Lang.exp}). *)

open Core
open Lang

exception IllTyped of exp
exception CannotUnify of (typ * typ) list

(** [infer gamma e] returns [tau] if and only if [e] has type [tau] in
    the datatype environment [sigma] and type environment [gamma] and throws the
    exception {!val:IllTyped} otherwise. *)
val infer : datatype_env -> typ_env -> exp -> typ

(** [check gamma e tau] returns [()] if and only if [e] has type [tau] in
    the datatype environment [sigma] and type environment [gamma] and throws the
    exception {!val:IllTyped} otherwise. *)
val check : datatype_env -> typ_env -> exp -> typ -> unit

(** [check_sub gamma e tau] returns [subst] if and only if [subst] unifies the
    type of [e] and the type [tau] in the datatype environment [sigma] and type
    environment [gamma] and throws the exception {!val:IllTyped} otherwise. *)
val check_sub : datatype_env -> typ_env -> exp -> typ -> Typ.sub

(** [well_typed (sigma, gamma, env)] returns [()] if and only if [env] is
    well-typed in the datatype environment [sigma] and type environment
    [gamma] and throws the exception {!val:IllTyped} otherwise. *)
val well_typed : datatype_env * typ_env * env -> unit

(** [unify constraints] returns a type substitition that unifies the left and
    right sides of each element of [constraints], or [None] if not possible. *)
val unify : (typ * typ) list -> Typ.sub option
