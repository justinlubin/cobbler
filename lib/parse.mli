(** Program parsing
    
    This module parses strings into programs (values using types in the
    [!module:Lang] module). *)

open Lang

(** [definitions s] parses a string [s] as a list of type and expression
    definitions. *)
val definitions : string -> typ_env * env

(** [exp s] parses a string [s] as an expression. *)
val exp : string -> exp
