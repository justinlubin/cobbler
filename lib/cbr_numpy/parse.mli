(** Program parsing: 
 * This module parses strings into IIR programs (using types from the [!module:Lang] module), 
 * using s-expressions as an intermediate representation. Also implements translation of
 * programs back into strings for printing and testing. *)

open Lang
open Core

val sexp_of_expr : expr -> Sexp.t
val expr_of_sexp : Sexp.t -> expr
val sexp_of_pat : pat -> Sexp.t
val sexp_of_block : block -> Sexp.t

(** [str_of_program] translates a program back into an s-expression, and then to a string. *)
val str_of_program : program -> string

(** [sexp_of_program] translates a program back into an s-expression **)
val sexp_of_program : program -> Sexp.t

(** [program_of_sexp] translates an s-expression into a program**)
val program_of_sexp : Sexp.t -> program

(** [sexp_of_substitions] translates a list of hole substitions back into an s-expression **)
val sexp_of_substitutions : substitutions -> Sexp.t

(** [substitions_of_sexp] translates an s-expression into a list of hole substitutions**)
val substitutions_of_sexp : Sexp.t -> substitutions

(** [program_of_str] translates an s-expression string into a program. **)
val program_of_str : string -> program

(** [pp_program] pretty-prints a program **)
val pp_program : ?channel:Out_channel.t -> program -> unit

(** [py_str_of_program] translates a program back into an s-expression, and then to a Python string. *)
val py_str_of_program : program -> string
