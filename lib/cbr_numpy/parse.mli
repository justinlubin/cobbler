(** Program parsing: 
 * This module parses strings into IIR programs (using types from the [!module:Lang] module), 
 * using s-expressions as an intermediate representation. Also implements translation of
 * programs back into strings for printing and testing. *)

open Lang
open Core

(** [parse_py] parses an s-expression string into a program.*)
val parse_py : string -> program

(** [str_of_program] translates a program back into an s-expression, and then to a string. *)
val str_of_program : program -> string

(** [sexp_of_program] translates a program back into an s-expression **)
val sexp_of_program : program -> Sexp.t

(** [program_of_str] translates an s-expression string into a program. **)
val program_of_str : string -> program
