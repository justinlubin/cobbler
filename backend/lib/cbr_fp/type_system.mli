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

(** [well_typed (sigma, gamma, env)] returns [()] if and only if [env] is
    well-typed in the datatype environment [sigma] and type environment
    [gamma] and throws the exception {!val:IllTyped} otherwise. *)
val well_typed : datatype_env * typ_env * env -> unit

(** [unify constraints] returns a type substitition that unifies the left and
    right sides of each element of [constraints], or [None] if not possible. *)
val unify : (typ * typ) list -> Typ.sub option

(** [assume_free_ok ~exceptions e gamma] returns a new type environment that
    extends [gamma] by assigning arbitrary (but fixed) types to the free
    variables in [e] (except for those in [exceptions]), which is useful (but
    unsound in general) if, for example, [e] is a snippet of a large
    codebase.  *)
val assume_free_ok
  :  exceptions:String.Set.t
  -> datatype_env
  -> exp
  -> typ_env
  -> typ_env
