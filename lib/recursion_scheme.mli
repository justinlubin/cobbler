(** Recursion schemes

    This module provides functions for working with recursion schemes such as
    catamorphisms. *)

open Lang

val extract_list_fold : recursive_name:string -> exp -> exp
