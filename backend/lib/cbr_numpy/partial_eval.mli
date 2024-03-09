(** Partial evaluation

    This module provides a function that performs partial evaluation
    on a {!val:Lang.program}. *)

open Lang

val partial_eval_expr : expr -> expr

(** [partial_eval_program p] partially evaluates [p]. *)
val partial_eval_program : program -> program
