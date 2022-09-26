open Core
open Lang

(* Comparator stuff *)

module T = struct
  type t = exp

  let compare = compare_exp
  let sexp_of_t = sexp_of_exp
end

include T
include Comparator.Make (T)

(* Normal stuff *)

let rec show : exp -> string = function
  | EVar id -> id
  | EApp (head, arg) -> sprintf "(%s %s)" (show head) (show arg)
  | EAbs (param, tau, body) ->
      sprintf "(lambda %s : %s -> %s)" param (Typ.show tau) (show body)
  | EMatch (scrutinee, branches) ->
      sprintf
        "(match %s %s)"
        (show scrutinee)
        (String.concat
           ~sep:" "
           (List.map
              ~f:(fun (ctor_name, (arg_name, rhs)) ->
                sprintf "(%s %s -> %s)" ctor_name arg_name (show rhs))
              branches))
  | ECtor (ctor_name, arg) -> sprintf "(%s %s)" ctor_name (show arg)
  | EPair (e1, e2) -> sprintf "(%s , %s)" (show e1) (show e2)
  | EFst arg -> sprintf "(fst %s)" (show arg)
  | ESnd arg -> sprintf "(snd %s)" (show arg)
  | EUnit -> "()"
  | EInt n -> string_of_int n
  | EHole (name, typ) -> sprintf "(?? %s %s)" name (Typ.show typ)

let map_branches : branch list -> f:(exp -> exp) -> branch list =
 fun branches ~f ->
  List.map branches ~f:(fun (ctor_name, (arg_name, rhs)) ->
      (ctor_name, (arg_name, f rhs)))

let decompose_abs : exp -> (id * typ) list * exp =
 fun e ->
  let rec decompose_abs' acc = function
    | EAbs (param, tau, body) -> decompose_abs' ((param, tau) :: acc) body
    | rest -> (List.rev acc, rest)
  in
  decompose_abs' [] e

let decompose_app : exp -> exp * exp list =
 fun t ->
  let rec decompose_app' acc = function
    | EApp (t1, t2) -> decompose_app' (t2 :: acc) t1
    | rest -> (rest, acc)
  in
  decompose_app' [] t

let build_abs : (id * typ) list -> exp -> exp =
 fun xs t ->
  List.fold_right xs ~init:t ~f:(fun (x, tau) acc -> EAbs (x, tau, acc))

let build_app : exp -> exp list -> exp =
 fun head args ->
  List.fold_left args ~init:head ~f:(fun acc arg -> EApp (acc, arg))

let rec free_variables : exp -> (id, String.comparator_witness) Set.t = function
  | EVar x -> Set.singleton (module String) x
  | EApp (head, arg) -> Set.union (free_variables head) (free_variables arg)
  | EAbs (param, _, body) -> Set.remove (free_variables body) param
  | EMatch (scrutinee, branches) ->
      Set.union_list
        (module String)
        (free_variables scrutinee
        :: List.map
             ~f:(fun (_, (arg_name, rhs)) ->
               Set.remove (free_variables rhs) arg_name)
             branches)
  | ECtor (_, arg) -> free_variables arg
  | EPair (e1, e2) -> Set.union (free_variables e1) (free_variables e2)
  | EFst arg -> free_variables arg
  | ESnd arg -> free_variables arg
  | EUnit | EInt _ | EHole (_, _) -> Set.empty (module String)

let replace : id * id -> exp -> exp =
 fun (lhs, rhs) e ->
  let rec replace' = function
    | EVar x -> if String.equal lhs x then EVar rhs else EVar x
    | EApp (head, arg) -> EApp (replace' head, replace' arg)
    | EAbs (param, tau, body) ->
        if String.equal lhs param
        then EAbs (rhs, tau, replace' body)
        else EAbs (param, tau, replace' body)
    | EMatch (scrutinee, branches) ->
        EMatch
          ( replace' scrutinee
          , List.map branches ~f:(fun (ctor_name, (arg_name, branch_rhs)) ->
                if String.equal lhs arg_name
                then (ctor_name, (rhs, replace' branch_rhs))
                else (ctor_name, (arg_name, replace' branch_rhs))) )
    | ECtor (ctor_name, arg) -> ECtor (ctor_name, replace' arg)
    | EPair (e1, e2) -> EPair (replace' e1, replace' e2)
    | EFst arg -> EFst (replace' arg)
    | ESnd arg -> ESnd (replace' arg)
    | EUnit -> EUnit
    | EInt n -> EInt n
    | EHole (name, typ) -> EHole (name, typ)
  in
  replace' e

let gensym_prefix : string = "var"

let substitute : id * exp -> exp -> exp =
 fun (lhs, rhs) e ->
  let rhs_fv = free_variables rhs in
  let rec substitute' = function
    | EVar x -> if String.equal lhs x then rhs else EVar x
    | EApp (head, arg) -> EApp (substitute' head, substitute' arg)
    | EAbs (param, tau, body) ->
        if String.equal lhs param
        then EAbs (param, tau, body)
        else if not (Set.mem rhs_fv param)
        then EAbs (param, tau, substitute' body)
        else (
          let new_param = Util.gensym gensym_prefix in
          EAbs (new_param, tau, substitute' (replace (param, new_param) body)))
    | EMatch (scrutinee, branches) ->
        EMatch
          ( substitute' scrutinee
          , List.map
              ~f:(fun (ctor_name, (arg_name, branch_rhs)) ->
                if String.equal lhs arg_name
                then (ctor_name, (arg_name, branch_rhs))
                else if not (Set.mem rhs_fv arg_name)
                then (ctor_name, (arg_name, substitute' branch_rhs))
                else (
                  let new_arg_name = Util.gensym gensym_prefix in
                  ( ctor_name
                  , ( new_arg_name
                    , substitute' (replace (arg_name, new_arg_name) branch_rhs)
                    ) )))
              branches )
    | ECtor (ctor_name, arg) -> ECtor (ctor_name, substitute' arg)
    | EPair (e1, e2) -> EPair (substitute' e1, substitute' e2)
    | EFst arg -> EFst (substitute' arg)
    | ESnd arg -> ESnd (substitute' arg)
    | EUnit -> EUnit
    | EInt n -> EInt n
    | EHole (name, typ) -> EHole (name, typ)
  in
  substitute' e

let freshen_exp : (id -> id) -> exp -> exp =
 fun renamer e ->
  let rec freshen_exp' = function
    | EVar x -> EVar x
    | EApp (head, arg) -> EApp (freshen_exp' head, freshen_exp' arg)
    | EAbs (param, tau, body) ->
        let new_param = renamer param in
        EAbs (new_param, tau, freshen_exp' (replace (param, new_param) body))
    | EMatch (scrutinee, branches) ->
        EMatch
          ( freshen_exp' scrutinee
          , List.map branches ~f:(fun (ctor_name, (arg_name, rhs)) ->
                let new_arg_name = renamer arg_name in
                ( ctor_name
                , ( new_arg_name
                  , freshen_exp' (replace (arg_name, new_arg_name) rhs) ) )) )
    | ECtor (ctor_name, arg) -> ECtor (ctor_name, freshen_exp' arg)
    | EPair (e1, e2) -> EPair (freshen_exp' e1, freshen_exp' e2)
    | EFst arg -> EFst (freshen_exp' arg)
    | ESnd arg -> ESnd (freshen_exp' arg)
    | EUnit -> EUnit
    | EInt n -> EInt n
    | EHole (name, typ) -> EHole (name, typ)
  in
  freshen_exp' e

let alpha_normalize : exp -> exp =
 fun e ->
  let suffix = ref (-1) in
  freshen_exp
    (fun _ ->
      suffix := !suffix + 1;
      gensym_prefix ^ Int.to_string !suffix)
    e

let alpha_equivalent : exp -> exp -> bool =
 fun e1 e2 -> [%eq: exp] (alpha_normalize e1) (alpha_normalize e2)

let rec normalize : exp -> exp = function
  | EVar id -> EVar id
  | EApp (head, arg) ->
      let arg' = normalize arg in
      (match normalize head with
      | EAbs (param, _, body) -> substitute (param, arg') body
      | head' -> EApp (head', arg'))
  | EAbs (param, tau, body) -> EAbs (param, tau, normalize body)
  | EMatch (scrutinee, branches) ->
      (match normalize scrutinee with
      | ECtor (ctor_name, arg) ->
          let arg_name, rhs =
            List.Assoc.find_exn ~equal:String.equal branches ctor_name
          in
          normalize (substitute (arg_name, arg) rhs)
      | scrutinee' -> EMatch (scrutinee', map_branches ~f:normalize branches))
  | ECtor (ctor_name, arg) -> ECtor (ctor_name, normalize arg)
  | EPair (e1, e2) -> EPair (normalize e1, normalize e2)
  | EFst arg ->
      (match normalize arg with
      | EPair (e1, _) -> e1
      | arg' -> EFst arg')
  | ESnd arg ->
      (match normalize arg with
      | EPair (_, e2) -> e2
      | arg' -> EFst arg')
  | EUnit -> EUnit
  | EInt n -> EInt n
  | EHole (name, typ) -> EHole (name, typ)
