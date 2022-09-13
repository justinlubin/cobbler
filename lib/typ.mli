(** Type utilities

    This module provides functions for working with {!val:Lang.typ}s. *)

open Core
open Lang

(** An alias for working with {!val:Lang.typ} *)
type t = Lang.typ

(** Comparator witness *)
type comparator_witness

(** Comparator  *)
val comparator : (t, comparator_witness) Base.Comparator.t

(** [show t] pretty-prints the type [t]. *)
val show : t -> string
