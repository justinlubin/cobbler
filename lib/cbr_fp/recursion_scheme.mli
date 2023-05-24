(** Recursion schemes

    This module provides functions for working with recursion schemes such as
    catamorphisms (folds). *)

open Lang

(** [rewrite sigma env name] tries to rewrite [env[name]] using recursion
    schemes as much as possible. *)
val rewrite : datatype_env -> env -> string -> exp option
