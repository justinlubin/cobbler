(** Top-level Elm synthesis algorithm

    This module contains the [solve] function which uses top down enumeration
    to find compositions of Elm Standard Library functions that are equivalent
    to the given Elm program input. *)

open Lang

(** [norm sigma gamma env e] returns a program [e'] that is semantically
    equivalent to [e] under [sigma], [gamma], and [env] but is defined in a
    restricted subset of the lambda calculus. This means that the equivalence
    classes of [norm] therefore conservatively underapproximate semantic
    equality, which means that to check whether a candidate program is
    semantically equal to a reference program, we can check whether their
    [norm]s are {i syntactically} equal (modulo alpha conversion); such a check
    is sound but not complete.

    **Note**: This is the *canonicalization function*. *)
val norm : datatype_env -> typ_env -> env -> exp -> exp

(* Specification for a synthesis problem. A solution to a synthesis problem
   [{sigma; gamma; env; name}] is an expression [e] that is:
   + semantically equivalent to [env[name]], and
   + defined in terms of expressions from the environment [env \ name] (typed by
     [sigma] and [gamma]). *)
type problem =
  { sigma : datatype_env
  ; gamma : typ_env
  ; env : env
  ; name : string
  }

(** [problem_of_definitions defs name] extracts the function named [name] from
    [defs] and returns the synthesis problem to rewrite it in terms of the
    remaining definitions. *)
val problem_of_definitions : datatype_env * typ_env * env -> string -> problem

(** [solve ~use_semantic_unification ~depth problem] tries to find a solution
    (and number of expansions used to find the solution) to [problem] using
    search depth [depth]. If [use_semantic_unification] is [true], the solver
    will use higher-order unification to fill out the leaves; otherwise, it will
    simply use naïve syntactic unification to do so. *)
val solve
  :  use_semantic_unification:bool
  -> depth:int
  -> problem
  -> (int * exp) option
