(** Common testing helper functions. *)

open Core
open Lib
open Lang

val parse_file : string -> datatype_env * typ_env * env
