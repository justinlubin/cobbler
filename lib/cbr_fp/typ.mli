(** Type utilities

    This module provides functions for working with {!val:Lang.typ}s. *)

open Core
open Lang

(** An alias for working with {!val:Lang.typ} *)
type t = Lang.typ

(** Comparator witness *)
type comparator_witness

(** Comparator  *)
val comparator : (t, comparator_witness) Base.Comparator.t

(** [show t] pretty-prints the type [t]. *)
val show : t -> string

(** [decompose_arr tau] strips the top-level arrows from [tau], so that if [tau]
    is [tau1 -> ... -> tauN -> tau'], then [decompose_arr tau] is
    [([tau1 ; ... ; tauN], tau')]. In other words, [decompose_arr] "undoes"
    {!val:build_arr}. *)
val decompose_arr : typ -> typ list * typ

(** [build_arr [tau1 ; ... ; tauN] tau'] returns the type
     [ tau1 -> ... -> tauN -> tau' ]. In other words, [build_app] "undoes"
     {!val:decompose_arr}. *)
val build_arr : typ list -> typ -> typ
