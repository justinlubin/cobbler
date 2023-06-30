(** Partial evaluation
    
    This module provides a function that performs partial evaluation
    on a {!val:Lang.program} *)

open Lang

val partial_eval_expr : expr -> expr

(** [partial_eval_program p] partially evaluates p by making 
    the following transformations:

    - len(mul(x,y)) -> len(x)
    - mul(x,y)[i] -> x[i] * y[i]
    
    *)
val partial_eval_program : program -> program

val substitute_in_expr : id -> expr -> expr -> expr
