(** Program parsing.
    
    This module parses strings into programs (values using types in the
    [!module:Lang] module). *)

open Lang

(** [definitions s] parses a string [s] as a list of type and expression
    definitions. *)
val definitions : string -> typ_env * env

(** [exp s] parses a string [s] as an expression. *)
val exp : string -> exp

(** [extract name (gamma, env)] removes (and returns) the [name] definition from
    [gamma] and [env]. *)
val extract : id -> typ_env * env -> typ_env * env * typ * exp

val extract_main_body
  :  typ_env * env
  -> typ_env * env * (id * typ) list * typ * exp
