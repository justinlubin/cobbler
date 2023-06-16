(** Program unparsing to strings understandable by elm-format

    This module unparses programs (values using types in the [!module:Lang]
    module) into strings that can be understood by elm-format v0.8.7. *)

open Lang

(* [typ t] unparses the type [t].*)
val typ : typ -> string

(* [exp e] unparses the expression [e].*)
val exp : exp -> string

(* [definition name t rhs] unparses the definitions whose name is [name], type
   is [t], and right-hand side is [rhs] .*)
val definition : string -> typ_scheme -> exp -> string
