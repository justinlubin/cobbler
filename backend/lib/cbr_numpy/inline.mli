(** Inlining transformation

    This module provides a function that inlines a {!val:Lang.program},
    replacing functions found in {!val:Lang.expr}s at the highest level. *)

open Lang

(** [inline_program (env, block)] inlines [block] by replacing
    {!val:Lang.expr}s at the highest level (i.e., those that do not have parent
    expression) that are calls to functions in [env]. *)
val inline_program : program -> program
