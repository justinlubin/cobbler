(** Enable or disable the ablation

    If [enable] is called, only syntactic unification will be performed. *)

(** Enables this module *)
val enable : unit -> unit

(** Returns whether or not this module is enabled *)
val enabled : unit -> bool
