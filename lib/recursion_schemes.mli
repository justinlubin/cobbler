(** Recursion schemes

    This module provides functions for working with recursion schemes such as
    catamorphisms. *)

open Lang

val make_cata : datatype:string -> constructors:(string * typ) list -> exp
