(** Top-level NumPy synthesis algorithm

    This module contains the [solve] function which uses top down enumeration
    to find NumPy code that is equivalent to a given Python program input.
*)

open Lang

(** Raised if it is known from the start that synthesis will fail, along with
    a description of why. *)
exception EarlyCutoff of string

(** [solve depth use_egraphs target] uses top down enumeration with search space
    of level [depth] to enumerate candidate program sketches, which are then
    canonicalized with partial evaluation and inlining and unified against
    [target] to determine a match. If any sketches match, the resulting
    substitution is made in the original sketch and outputed along with the
    number of expansions used to find the sketch. If no solution is found,
    [None] is returned. *)
val solve
  :  int
  -> ?debug:bool
  -> use_egraphs:bool
  -> program
  -> (int * program) option

(** The canonicalization function *)
val canonicalize : program -> program
