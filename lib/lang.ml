(** Language module

    This module includes all the type definitions necessary to interoperate
    between all other modules. In particular, it provides the type definitions
    for all concepts related to the language definition. *)

open Base

(** Identifiers *)
type id = string

(** Types *)
and typ =
  | TPlaceholder of string
  | TArr of typ * typ
[@@deriving sexp, ord, eq, compare]

(** An environment of types (commonly called "gamma") *)
type typ_env = (id, typ, String.comparator_witness) Map.t

(** Case branches *)
type branch = string * (id * exp)

(** Expressions *)
and exp =
  | EVar of id
  | EApp of exp * exp
  | EAbs of id * typ * exp
  | EMatch of exp * branch list
  | ECtor of string * exp
  | EInt of int
  | EHole of typ
[@@deriving sexp, ord, eq, compare]

(** An environment of expressions *)
type env = (id, exp, String.comparator_witness) Map.t
