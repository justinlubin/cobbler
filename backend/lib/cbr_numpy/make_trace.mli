(** Make_trace

    This module contains the [make_trace] function that generates a program trace.
*)

open Lang
open Env
open Yojson.Basic

(** [make_trace p] creates the program trace for subexpressions of program [p] *)
val make_trace : program -> trace

val make_json_trace_of_program : program -> Yojson.Basic.t