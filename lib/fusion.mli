(** Expression fusion

    Fusion removes intermediate allocations in expressions, but it also provides
    a nice way to further normalize expressions beyond typical standard
    alpha/beta-normalization. *)

open Lang

(** [pull_out_cases e] reorders case expressions in [e] (which must be closed)
    such that there are no case expressions in [e] whose scrutinee is itself a
    case expression. See "Deforestation: Transforming programs to eliminate
    trees" (Wadler 1988).*)
val pull_out_cases : exp -> exp

(** [case_normalize e] recursively reduces all case redexes (case expressions
    whose scrutinees are constructor literals) in [e]. *)
val case_normalize : exp -> exp
