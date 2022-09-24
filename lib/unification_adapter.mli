(** Translation between {!val:Lang.exp} and {!val:Unification.term} *)

(** [to_unification_term sigma gamma e] translates the language expression [e]
    to an equivalent unification term under the datatype environment [sigma] and
    standard library [gamma]. *)
val to_unification_term
  :  Lang.datatype_env
  -> Lang.typ_env
  -> Lang.exp
  -> Unification.term

(** Assuming [t] is in normal form, [from_unification_term sigma t] translates
    the unification term [t] to an equivalent language expression under the
    datatype environment [sigma]. *)
val from_unification_term : Lang.datatype_env -> Unification.term -> Lang.exp
