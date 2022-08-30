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

(** Constructor tags (names) *)
type tag = string

(** Case branches *)
and branch = tag * (id * exp)

(** Expressions *)
and exp =
  | EVar of id
  | EApp of exp * exp
  | EAbs of id * exp
  | EMatch of exp * branch list
  | ECtor of tag * exp
  | EInt of int
  | EHole of typ
[@@deriving sexp, ord, eq, compare]

(** An environment of expressions *)
type env = (id, exp, String.comparator_witness) Map.t

(** Useful as a comparator module for exps *)
module Exp = struct
  module T = struct
    type t = exp

    let compare = compare_exp
    let sexp_of_t = sexp_of_exp
  end

  include T
  include Comparator.Make (T)
end

(** Useful as a comparator module for typs *)
module Typ = struct
  module T = struct
    type t = typ

    let compare = compare_typ
    let sexp_of_t = sexp_of_typ
  end

  include T
  include Comparator.Make (T)
end
