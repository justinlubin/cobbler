(** Program parsing.
    
    This module parses strings into programs (values using types in the
    [!module:Lang] module). *)

open Lang

(** [program s] parses a string as a list of definitions and extracts the "main"
    definition. *)
val program : string -> env * exp
