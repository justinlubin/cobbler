(** Language module

    This module includes all the type definitions necessary to interoperate
    between all other modules. In particular, it provides the type definitions
    for all concepts related to the language definition. *)

open Core

(** Identifiers *)
type id = string

(** Types *)
and typ =
  | TUnit
  | TInt
  | TDatatype of string
  | TProd of typ * typ
  | TArr of typ * typ
[@@deriving sexp, ord, eq, compare, show]

(** An environment of types (commonly called "gamma") *)
type typ_env = (id, typ, String.comparator_witness) Map.t

(** An environment of datatypes (commonly called "sigma") *)
type datatype_env =
  (string, (string * typ) list, String.comparator_witness) Map.t

(** Case branches *)
type branch = string * (id * exp)

(** Recursion schemes *)
and rscheme = RListFoldr of exp * exp

(** Expressions *)
and exp =
  | EVar of id
  | EApp of exp * exp
  | EAbs of id * typ * exp
  | EMatch of exp * branch list
  | ECtor of string * exp
  | EPair of exp * exp
  | EFst of exp
  | ESnd of exp
  | EUnit
  | EInt of int
  | EHole of string * typ
  | ERScheme of rscheme * exp
[@@deriving sexp, ord, eq, compare, show]

(** An environment of expressions *)
type env = (id, exp, String.comparator_witness) Map.t
