(** Recursion schemes

    This module provides functions for working with recursion schemes such as
    catamorphisms (folds). *)

open Lang

(** [extract_list_foldr sigma gamma env name] tries to rewrite [env[name]] using
    a lists foldr. *)
val extract_list_foldr : datatype_env -> typ_env -> env -> string -> exp option
