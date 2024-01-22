open Core
open Lang

val partial_eval_expr_with_trace : expr -> expr * expr list

val partial_eval_program_with_trace : program * program list -> program * program list