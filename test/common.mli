(** Common testing helper functions. *)

open Core
open Lib
open Lang

val parse_file : string -> typ_env * env
