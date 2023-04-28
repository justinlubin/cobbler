(** Np_synthesis

    This module contains the [solve] function which uses top down enumeration
    to find numpy code that is equivalent to a given python program input.
*)

open Lang

(** [solve depth program_type target] uses top down enumeration with search space of level [depth]
    to enumerate candidate program sketches, which are then canonicalized with partial eval
    and inlining and unified against [target] to determine a match. If any sketches match,
    the resulting substitution is made in the original sketch and outputed. If no solution
    is found, [None] is returned. The starting sketch is a hole with type [program_type].
*)
val solve : int -> hole_type -> program -> program option
