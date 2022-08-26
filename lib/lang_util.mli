(** Language utilities.

    This module provides helper functions specifically for working with the
    types defined in [!module:Lang]. *)

open Lang

(** [closed_substitute env e] substitutes all bindings in [env] in [e],
    requiring that [env] is closed. *)
val closed_substitute : env -> exp -> exp

(** [map_branches branches ~f] applies [f] to the right-hand sides of each
    branch in [branches]. *)
val map_branches : branch list -> f:(exp -> exp) -> branch list
