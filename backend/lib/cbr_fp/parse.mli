(** Program parsing

    This module parses strings of s-expressions into programs (values using
    types in the [!module:Lang] module).

    **Note**: The more useful and feature-complete module is
    [!module:Parse_json], which parses JSON strings. *)

open Lang

(** [definitions s] parses a string [s] as a list of datatype, type, and
    expression definitions. *)
val definitions : string -> datatype_env * typ_env * env

(** [exp s] parses a string [s] as an expression. *)
val exp : string -> exp

(** [typ s] parses a string [s] as a type. *)
val typ : string -> typ
