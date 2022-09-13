open Core
open Lang

(* Comparator stuff *)

module T = struct
  type t = typ

  let compare = compare_typ
  let sexp_of_t = sexp_of_typ
end

include T
include Comparator.Make (T)

(* Normal stuff *)

let rec show : typ -> string = function
  | TPlaceholder x -> x
  | TArr (domain, codomain) ->
      sprintf "(%s -> %s)" (show domain) (show codomain)
