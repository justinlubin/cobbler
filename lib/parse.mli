(** Program parsing.
    
    This module parses strings into programs (values using types in the
    [!module:Lang] module). *)

open Lang

(** [program s] parses a string [s] as a list of type and expression definitions
    and extracts the "main" expression. *)
val program : string -> typ_env * env * exp

(** [exp s] parses a string [s] as an expression. *)
val exp : string -> exp
