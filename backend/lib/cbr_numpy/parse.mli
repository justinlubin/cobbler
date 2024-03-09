(** Program parsing:

    This module parses strings [!module:Lang] programs , using s-expressions as
    an intermediate representation.

    This module also implements translation of programs back into strings
    (unparsing) for printing and testing.

     Upon parse failure, this module throws the [ParseFail] exception. *)

open Lang
open Core

(** Raised on parse failure *)
exception ParseFail of string

(** Parse an s-expression to an expression *)
val expr_of_sexp : Sexp.t -> expr

(** Unparse an expression to an s-expressions *)
val sexp_of_expr : expr -> Sexp.t

(** Parse an s-expression to a program *)
val program_of_sexp : Sexp.t -> program

(* Unparse a program to an s-expression *)
val sexp_of_program : program -> Sexp.t

(** Parse an s-expression string into a program **)
val program_of_str : string -> program

(** Unparse a program to an s-expression string (see also
    [py_str_of_program]) *)
val str_of_program : program -> string

(** Unparse a program to a Python string *)
val py_str_of_program : program -> string

(** Pretty-prints a program **)
val pp_program : ?channel:Out_channel.t -> program -> unit
