(** Common testing helper functions. *)

open Core
open Cbr_fp
open Lang

val parse_file : string -> datatype_env * typ_env * env
val parse_file_json : string -> datatype_env * typ_env * env
