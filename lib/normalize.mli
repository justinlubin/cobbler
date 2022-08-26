(** Expression normalization.
    
    The key export of this module is the [full] function, which fully normalizes
    an expression. This is useful for doing a lightweight program equality
    test: to check if programs [e1] and [e2] are semantically equivalent in an
    environment [env], simply check whether [full env e1] is {i syntactically}
    equivalent to [full env e2]. If so, they are definitely semantically equal;
    if not, they may or may not be semantically equal. (This procedure is
    {i sound} but not complete). *)

open Lang

(** [reduce e] reduces {i all} beta redexes in [e] (which must be closed),
    including those under abstractions. *)
val fully_reduce : exp -> exp

(** [pull_out_cases e] reorders case expressions in [e] (which must be closed)
    such that there are no case expressions in [e] whose scrutinee is itself a
    case expression. *)
val pull_out_cases : exp -> exp

(** [partially_evaluate_cases e] simplifies all case expressions in [e] (which
    must be closed) whose scrutinee is a constructor literal. *)
val partially_evaluate_cases : exp -> exp

(** [full env e] inlines [env] (which must be closed) into [e] (which must be
    closed under [env]) then runs the entire normalization pipeline on the
    resulting expression. *)
val full : env -> exp -> exp
