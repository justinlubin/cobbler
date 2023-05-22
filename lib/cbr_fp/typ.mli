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

(** [ctor_typ sigma tag] looks up the type of the constructor [tag] in
    [sigma], returning its datatype (with a list of parameter names) and
    its argument type list. *)
val ctor_typ
  :  datatype_env
  -> string
  -> ((string * string list) * typ list) option

(* Type substitutions (mappings of free variables to types) *)
type sub = typ String.Map.t

(** [apply_sub subst tau] applies the type substitute [subst] to the type
    [tau]. *)
val apply_sub : sub -> typ -> typ

(** [compose subst1 subst2] composes the substitutions [subst1] and [subst2],
    resulting in a new substitution that is equivalent to first applying
    [subst2] then applying [subst1]. *)
val compose_subs : sub -> sub -> sub

(** [free_vars tau] returns the free variables of [tau]. Since there are no
    type binders, this function returns all the type variables present in
    [tau]. *)
val free_vars : typ -> String.Set.t

(** [fresh_type_var ()] creates a fresh type variable, guaranteed to be
    unique. *)
val fresh_type_var : unit -> typ

(** [instantiate ts] instantiates a type scheme, i.e. replaces the forall
    quantifications with fresh free variables. (In a sense, the opposite to
    [generalize].) *)
val instantiate : typ_scheme -> typ

(** [generalize t] generalizes a type over its free variables by introducing
    forall quantifications. (In a sense, the opposite to [instantiate].)
    This is useful for generalizing top-level definitions to be polymorphic. *)
val generalize : typ -> typ_scheme
