(** Np_synthesis

    This module contains the [solve] function which uses top down enumeration
    to find numpy code that is equivalent to a given python program input.
*)

open Lang

(** Raised if it is known from the start that synthesis will fail, along with
    a description of why. *)
exception EarlyCutoff of string

(** [solve depth target] uses top down enumeration with search space of level [depth]
    to enumerate candidate program sketches, which are then canonicalized with partial eval
    and inlining and unified against [target] to determine a match. If any sketches match,
    the resulting substitution is made in the original sketch and outputed. If no solution
    is found, [None] is returned.
*)
val solve : int -> ?debug:bool -> program -> bool -> program option

val canonicalize : program -> program
