(** Common testing helper functions. *)

open Core
open Cbr_fp
open Lang

val parse_file : string -> datatype_env * typ_env * env
