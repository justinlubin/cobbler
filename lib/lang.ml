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
  | TArr of typ * typ
[@@deriving sexp, ord, eq, compare]

(** An environment of types (commonly called "gamma") *)
type typ_env = (id, typ, String.comparator_witness) Map.t

(** An environment of datatypes (commonly called "sigma") *)
type datatype_env =
  (string, (string * typ) list, String.comparator_witness) Map.t

(** Case branches *)
type branch = string * (id * exp)

(** Expressions *)
and exp =
  | EVar of id
  | EApp of exp * exp
  | EAbs of id * typ * exp
  | EMatch of exp * branch list
  | ECtor of string * exp
  | EUnit
  | EInt of int
  | EHole of string * typ
[@@deriving sexp, ord, eq, compare]

(** An environment of expressions *)
type env = (id, exp, String.comparator_witness) Map.t

(** TODO: temporary! *)
let default_datatype_env : datatype_env =
  String.Map.of_alist_exn
    [ ("MaybePeano", [ ("Nothing", TUnit); ("Just", TDatatype "Peano") ])
    ; ("Gamma", [ ("B", TUnit) ])
    ]
