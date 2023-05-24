(** Recursion schemes

    This module provides functions for working with recursion schemes such as
    catamorphisms (folds). *)

open Lang

(** [extract_cata sigma gamma env name] tries to rewrite [env[name]] using
    a catamorphism. *)
val extract_cata : datatype_env -> typ_env -> env -> string -> exp option
