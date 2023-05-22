(** Language module

    This module includes all the type definitions necessary to interoperate
    between all other modules. In particular, it provides the type definitions
    for all concepts related to the language definition. *)

open Core

(** Base types *)
type base_typ =
  | BTInt
  | BTString
  | BTFloat

(** Types *)
and typ =
  | TBase of base_typ
  | TVar of string
  | TDatatype of string * typ list
  | TArr of typ * typ
[@@deriving sexp, ord, eq, compare, show]

(** An environment of types (commonly called "gamma") *)
type typ_env = typ String.Map.t

(** An environment of datatypes (commonly called "sigma") *)
type datatype_env = (string list * (string * typ list) list) String.Map.t

(** Case branches *)
type branch = string * (string list * exp)

(** Recursion schemes *)
and rscheme = RListFoldr of exp * exp

(** Base expressions *)
and base_exp =
  | BEInt of int
  | BEString of string
  | BEFloat of float

(** Expressions *)
and exp =
  | EVar of string
  | EApp of exp * exp
  | EAbs of string * typ * exp
  | EMatch of exp * branch list
  | ECtor of string * exp list
  | EBase of base_exp
  | EHole of string * typ
  | ERScheme of rscheme
[@@deriving sexp, ord, eq, compare, show]

(** An environment of expressions *)
type env = exp String.Map.t
