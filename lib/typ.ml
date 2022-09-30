open Core
open Lang

module T = struct
  type t = typ

  let compare = compare_typ
  let sexp_of_t = sexp_of_typ
end

include T
include Comparator.Make (T)

let rec show : typ -> string = function
  | TUnit -> "Unit"
  | TInt -> "Int"
  | TDatatype x -> x
  | TProd (tau1, tau2) -> sprintf "(%s * %s)" (show tau1) (show tau2)
  | TArr (domain, codomain) ->
      sprintf "(%s -> %s)" (show domain) (show codomain)

let rec decompose_arr : typ -> typ list * typ = function
  | TUnit -> ([], TUnit)
  | TInt -> ([], TInt)
  | TDatatype x -> ([], TDatatype x)
  | TProd (tau1, tau2) -> ([], TProd (tau1, tau2))
  | TArr (domain, codomain) ->
      let domain', codomain' = decompose_arr codomain in
      (domain :: domain', codomain')

let rec build_arr : typ list -> typ -> typ =
 fun domain codomain ->
  match domain with
  | [] -> codomain
  | hd :: tl -> TArr (hd, build_arr tl codomain)
