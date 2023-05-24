(** Expression fusion

    Fusion removes intermediate allocations in expressions, but it also provides
    a nice way to further normalize expressions beyond typical standard
    alpha/beta-normalization. *)

open Lang

(** [fuse sigma e] performs recursion scheme fusion in all applicable
    subexpressions of [e]. For example, the list foldr fusion law is:
      For all x, acc,
        u . foldr b f = foldr (u b) h
      where
        h (x, u acc) = u (f (x, acc)).
    So this function will try to replace instances of the left-hand side above
    with the right-hand side (note that this specification does not provide a
    constructive definition for [h]).

    This function subsumes the deforestation transformation (Wadler 1988). *)
val fuse : datatype_env -> exp -> exp
