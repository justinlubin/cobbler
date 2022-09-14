(** Translation between {!val:Lang.exp} and {!val:Unification.term} *)

(** [to_unification_term e] translates the language expression [e] to an
    equivalent unification term. *)
val to_unification_term : Lang.exp -> Unification.term

(** [from_unification_term t] translates the unification term [t] to an
    equivalent language expression. *)
val from_unification_term : Unification.term -> Lang.exp
