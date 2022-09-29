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

(** [fuse e] performs recursive scheme fusion in all applicable subexpressions
    of [e]. For example, the list foldr fusion law is:
      For all x, acc,
        u . foldr b f = foldr (u b) h
      where
        h (x, u acc) = u (f (x, acc)).
    So this function will try to replace instances of the left-hand side above
    with the right-hand side (note that this specification does not provide a
    constructive definition for [h]). *)
val fuse : exp -> exp
