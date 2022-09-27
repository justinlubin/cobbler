(** Recursion schemes

    This module provides functions for working with recursion schemes such as
    catamorphisms. *)

open Lang

val extract_list_foldr : datatype_env -> typ_env -> env -> string -> exp
