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
  | TInt -> "Int"
  | TVar x -> x
  | TDatatype (x, taus) ->
      sprintf
        "(%s%s)"
        x
        (taus |> List.map ~f:(fun t -> " " ^ show t) |> String.concat)
  | TArr (domain, codomain) ->
      sprintf "(%s -> %s)" (show domain) (show codomain)

let rec decompose_arr : typ -> typ list * typ = function
  | TInt -> ([], TInt)
  | TVar x -> ([], TVar x)
  | TDatatype (x, taus) -> ([], TDatatype (x, taus))
  | TArr (domain, codomain) ->
      let domain', codomain' = decompose_arr codomain in
      (domain :: domain', codomain')

let rec build_arr : typ list -> typ -> typ =
 fun domain codomain ->
  match domain with
  | [] -> codomain
  | hd :: tl -> TArr (hd, build_arr tl codomain)
