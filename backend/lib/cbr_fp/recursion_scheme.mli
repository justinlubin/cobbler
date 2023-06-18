(** Recursion schemes

    This module provides functions for working with recursion schemes such as
    catamorphisms (folds). *)

open Lang

(** [rewrite sigma name e] tries to rewrite [e] (assuming its name is [name])
    using recursion schemes as much as possible. *)
val rewrite : datatype_env -> string -> exp -> exp
