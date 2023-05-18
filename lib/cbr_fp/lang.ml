(** Language module

    This module includes all the type definitions necessary to interoperate
    between all other modules. In particular, it provides the type definitions
    for all concepts related to the language definition. *)

open Core

(** Identifiers *)
type id = string

(** Types *)
and typ =
  | TInt
  | TVar of string
  | TDatatype of string * typ list
  | TArr of typ * typ
[@@deriving sexp, ord, eq, compare, show]

(** An environment of types (commonly called "gamma") *)
type typ_env = (id, typ, String.comparator_witness) Map.t

(** An environment of datatypes (commonly called "sigma") *)
type datatype_env =
  ( string
  , string list * (string * typ list) list
  , String.comparator_witness )
  Map.t

(** Case branches *)
type branch = string * (id list * exp)

(** Recursion schemes *)
and rscheme = RListFoldr of exp * exp

(** Expressions *)
and exp =
  | EVar of id
  | EApp of exp * exp
  | EAbs of id * typ * exp
  | EMatch of exp * branch list
  | ECtor of string * exp list
  | EInt of int
  | EHole of string * typ
  | ERScheme of rscheme
[@@deriving sexp, ord, eq, compare, show]

(** An environment of expressions *)
type env = (id, exp, String.comparator_witness) Map.t
