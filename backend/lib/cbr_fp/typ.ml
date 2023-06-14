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
  | TBase BTInt -> "Int"
  | TBase BTFloat -> "Float"
  | TBase BTString -> "String"
  | TVar x -> x
  | TDatatype (x, taus) ->
      sprintf
        "(%s%s)"
        x
        (taus |> List.map ~f:(fun t -> " " ^ show t) |> String.concat)
  | TArr (domain, codomain) ->
      sprintf "(%s -> %s)" (show domain) (show codomain)

let rec decompose_arr : typ -> typ list * typ = function
  | TBase b -> ([], TBase b)
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

let ctor_typ
    : datatype_env -> string -> ((string * string list) * typ list) option
  =
 fun sigma tag ->
  List.find_map (Map.to_alist sigma) ~f:(fun (dt, (dt_params, dt_info)) ->
      Option.map
        (List.Assoc.find dt_info ~equal:String.equal tag)
        ~f:(fun domains -> ((dt, dt_params), domains)))

let rec free_vars : typ -> String.Set.t =
 fun tau ->
  match tau with
  | TBase _ -> String.Set.empty
  | TVar x -> String.Set.singleton x
  | TDatatype (_, args) ->
      args |> List.map ~f:free_vars |> String.Set.union_list
  | TArr (dom, cod) -> Set.union (free_vars dom) (free_vars cod)

let fresh_type_var : unit -> typ = fun () -> TVar (Util.gensym "__typevar")

(* Type substitions (no need to worry about variable capture since we don't
   have fully-fledged universal polymorphism) *)

type sub = typ String.Map.t

let rec apply_sub : sub -> typ -> typ =
 fun sigma tau ->
  match tau with
  | TBase b -> TBase b
  | TVar x ->
      (match Map.find sigma x with
      | None -> TVar x
      | Some t -> t)
  | TDatatype (dt, args) -> TDatatype (dt, List.map ~f:(apply_sub sigma) args)
  | TArr (dom, cod) -> TArr (apply_sub sigma dom, apply_sub sigma cod)

let compose_subs : sub -> sub -> sub =
 fun sigma1 sigma2 ->
  Map.merge sigma1 sigma2 ~f:(fun ~key el ->
      match el with
      | `Left t1 -> Some t1
      | `Right t2 | `Both (_, t2) -> Some (apply_sub sigma1 t2))

(* Type schemes *)

let instantiate : typ_scheme -> typ =
 fun (xs, t) ->
  apply_sub
    (String.Map.of_alist_exn (List.map ~f:(fun x -> (x, fresh_type_var ())) xs))
    t

let generalize : typ -> typ_scheme =
 fun t ->
  let fvs = free_vars t in
  (Set.to_list fvs, t)
