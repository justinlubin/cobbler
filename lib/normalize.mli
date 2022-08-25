(** Expression normalization.
    
    The key export of this module is the [full] function, which fully normalizes
    an expression. This is useful for doing a lightweight program equality
    test: to check if programs [e1] and [e2] are semantically equivalent in a
    library [lib], simply check whether [full lib e1] is {i syntactically}
    equivalent to [full lib e2]. If so, they are definitely semantically equal;
    if not, they may or may not be semantically equal. (This procedure is
    {i sound} but not complete). *)

open Lang

(** [inline lib e] inlines all expressions defined in [env] into the expression
    [e]. *)
val inline : env -> exp -> exp

(** [partially_evaluate_cases e] simplifies all case expressions in [e] whose
    scrutinee is a constructor literal. *)
val partially_evaluate_cases : exp -> exp

(** [pull_out_cases e] reorders case expressions in [e] such that there are
    no case expressions in [e] whose scrutinee is itself a case expression. *)
val pull_out_cases : exp -> exp

(** [all env e] runs the entire normalization pipeline on [e] using the
    environment [env]. *)
val full : env -> exp -> exp
