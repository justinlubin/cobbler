(** Top-level synthesis algorithm *)

open Lang

(** [norm env e] returns a program [e'] that is semantically equivalent to [e] 
    under [exp] but is defined in a restricted subset of the lambda calculus.
    This means that The equivalence classes of [norm] therefore conservatively
    underapproximate semantic equality, which means that to check whether a
    candidate program is semantically equal to a reference program, we can check
    whether their [norm]s are {i syntactically} equal (modulo alpha conversion);
    such a check is sound but not complete. *)
val norm : env -> exp -> exp

(* Specification for a synthesis problem. A solution to a synthesis problem
   [{gamma; env; free_vars; goal_typ; reference}] is an expression [e] that is:
   + semantically equivalent to [reference], and
   + defined in terms of expressions from the environment [env] (typed by
     [gamma]).
   Both [reference] and [e] are of type [goal_typ] and have free variables
   [free_vars]. *)
type problem =
  { gamma : typ_env
  ; env : env
  ; free_vars : (id * typ) list
  ; goal_typ : typ
  ; reference : exp
  }

(** [problem_of_definitions defs] extracts the function named "main" from
    [defs] and returns the synthesis problem to rewrite it in terms of the
    remaining definitions. *)
val problem_of_definitions : typ_env * env -> problem

(** [solve problem] tries to find a solution to [problem]. *)
val solve : problem -> exp option
