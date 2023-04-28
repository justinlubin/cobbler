open Core
open Lang

module T = struct
  type t = exp

  let compare = compare_exp
  let sexp_of_t = sexp_of_exp
end

include T
include Comparator.Make (T)

let rec show_single : exp -> string =
 fun e ->
  match e with
  | EVar id -> id
  | EApp (head, arg) -> sprintf "(%s %s)" (show_single head) (show_single arg)
  | EAbs (param, tau, body) ->
      sprintf "(lambda %s %s %s)" param (Typ.show tau) (show_single body)
  | EMatch (scrutinee, branches) ->
      sprintf
        "(match %s %s)"
        (show_single scrutinee)
        (String.concat
           ~sep:" "
           (List.map
              ~f:(fun (ctor_name, (arg_name, rhs)) ->
                sprintf "(%s %s -> %s)" ctor_name arg_name (show_single rhs))
              branches))
  | ECtor (ctor_name, arg) -> sprintf "(%s %s)" ctor_name (show_single arg)
  | EPair (e1, e2) -> sprintf "(%s , %s)" (show_single e1) (show_single e2)
  | EFst arg -> sprintf "(fst %s)" (show_single arg)
  | ESnd arg -> sprintf "(snd %s)" (show_single arg)
  | EUnit -> "()"
  | EInt n -> string_of_int n
  | EHole (name, typ) -> sprintf "(?? %s %s)" name (Typ.show typ)
  | ERScheme (RListFoldr (b, f)) ->
      sprintf "(list_foldr %s %s)" (show_single b) (show_single f)

let rec show_multi : int -> exp -> string =
 fun depth e ->
  let single_indent = "  " in
  let indent = String.concat (List.init depth ~f:(fun _ -> single_indent)) in
  indent
  ^
  match e with
  | EVar id -> id
  | EApp (head, arg) -> sprintf "(%s %s)" (show_single head) (show_single arg)
  | EAbs (param, tau, body) ->
      sprintf
        "(lambda %s %s\n%s)"
        param
        (Typ.show tau)
        (show_multi (depth + 1) body)
  | EMatch (scrutinee, branches) ->
      sprintf
        "(match %s\n%s)"
        (show_single scrutinee)
        (String.concat
           ~sep:"\n"
           (List.map
              ~f:(fun (ctor_name, (arg_name, rhs)) ->
                sprintf
                  "%s%s(%s %s ->\n%s)"
                  indent
                  single_indent
                  ctor_name
                  arg_name
                  (show_multi (depth + 2) rhs))
              branches))
  | ECtor (ctor_name, arg) -> sprintf "(%s %s)" ctor_name (show_single arg)
  | EPair (e1, e2) -> sprintf "(%s , %s)" (show_single e1) (show_single e2)
  | EFst arg -> sprintf "(fst %s)" (show_single arg)
  | ESnd arg -> sprintf "(snd %s)" (show_single arg)
  | EUnit -> "()"
  | EInt n -> string_of_int n
  | EHole (name, typ) -> sprintf "(?? %s %s)" name (Typ.show typ)
  | ERScheme (RListFoldr (b, f)) ->
      sprintf "(list_foldr %s %s)" (show_single b) (show_single f)

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

let rec free_variables : exp -> String.Set.t = function
  | EVar x -> String.Set.singleton x
  | EApp (head, arg) -> Set.union (free_variables head) (free_variables arg)
  | EAbs (param, _, body) -> Set.remove (free_variables body) param
  | EMatch (scrutinee, branches) ->
      String.Set.union_list
        (free_variables scrutinee
        :: List.map
             ~f:(fun (_, (arg_name, rhs)) ->
               Set.remove (free_variables rhs) arg_name)
             branches)
  | ECtor (_, arg) -> free_variables arg
  | EPair (e1, e2) -> String.Set.union (free_variables e1) (free_variables e2)
  | EFst arg -> free_variables arg
  | ESnd arg -> free_variables arg
  | ERScheme (RListFoldr (b, f)) ->
      String.Set.union (free_variables b) (free_variables f)
  | EUnit | EInt _ | EHole (_, _) -> String.Set.empty

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
    | ERScheme (RListFoldr (b, f)) ->
        ERScheme (RListFoldr (replace' b, replace' f))
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
    | ERScheme (RListFoldr (b, f)) ->
        ERScheme (RListFoldr (substitute' b, substitute' f))
  in
  substitute' e

let freshen_with : (id -> id) -> exp -> exp =
 fun renamer e ->
  let rec freshen_with' = function
    | EVar x -> EVar x
    | EApp (head, arg) -> EApp (freshen_with' head, freshen_with' arg)
    | EAbs (param, tau, body) ->
        let new_param = renamer param in
        EAbs (new_param, tau, freshen_with' (replace (param, new_param) body))
    | EMatch (scrutinee, branches) ->
        EMatch
          ( freshen_with' scrutinee
          , List.map branches ~f:(fun (ctor_name, (arg_name, rhs)) ->
                let new_arg_name = renamer arg_name in
                ( ctor_name
                , ( new_arg_name
                  , freshen_with' (replace (arg_name, new_arg_name) rhs) ) )) )
    | ECtor (ctor_name, arg) -> ECtor (ctor_name, freshen_with' arg)
    | EPair (e1, e2) -> EPair (freshen_with' e1, freshen_with' e2)
    | EFst arg -> EFst (freshen_with' arg)
    | ESnd arg -> ESnd (freshen_with' arg)
    | EUnit -> EUnit
    | EInt n -> EInt n
    | EHole (name, typ) -> EHole (name, typ)
    | ERScheme (RListFoldr (b, f)) ->
        ERScheme (RListFoldr (freshen_with' b, freshen_with' f))
  in
  freshen_with' e

let freshen : exp -> exp = freshen_with (fun _ -> Util.gensym gensym_prefix)

let alpha_normalize : exp -> exp =
 fun e ->
  let suffix = ref (-1) in
  freshen_with
    (fun _ ->
      suffix := !suffix + 1;
      gensym_prefix ^ Int.to_string !suffix)
    e

let alpha_equivalent : exp -> exp -> bool =
 fun e1 e2 -> [%eq: exp] (alpha_normalize e1) (alpha_normalize e2)

let rec normalize : exp -> exp = function
  | EVar id -> EVar id
  | EApp (head, arg) ->
      (match (normalize head, normalize arg) with
      | EAbs (param, _, body), arg' -> substitute (param, arg') body
      | ERScheme (RListFoldr (b, f)), ECtor ("Nil", EUnit) -> b
      | ERScheme (RListFoldr (b, f)), ECtor ("Cons", EPair (hd, tl)) ->
          normalize
            (EApp (f, EPair (hd, EApp (ERScheme (RListFoldr (b, f)), tl))))
      | head', arg' -> EApp (head', arg'))
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
      | arg' -> ESnd arg')
  | EUnit -> EUnit
  | EInt n -> EInt n
  | EHole (name, typ) -> EHole (name, typ)
  | ERScheme (RListFoldr (b, f)) ->
      ERScheme (RListFoldr (normalize b, normalize f))

let replace_subexp : old_subexp:exp -> new_subexp:exp -> exp -> exp =
 fun ~old_subexp ~new_subexp e ->
  let rec recurse = function
    | EVar x -> EVar x
    | EApp (head, arg) -> EApp (check_and_replace head, check_and_replace arg)
    | EAbs (param, tau, body) -> EAbs (param, tau, check_and_replace body)
    | EMatch (scrutinee, branches) ->
        EMatch
          ( check_and_replace scrutinee
          , map_branches ~f:check_and_replace branches )
    | ECtor (ctor_name, arg) -> ECtor (ctor_name, check_and_replace arg)
    | EPair (e1, e2) -> EPair (check_and_replace e1, check_and_replace e2)
    | EFst arg -> EFst (check_and_replace arg)
    | ESnd arg -> ESnd (check_and_replace arg)
    | EUnit -> EUnit
    | EInt n -> EInt n
    | EHole (name, typ) -> EHole (name, typ)
    | ERScheme (RListFoldr (b, f)) ->
        ERScheme (RListFoldr (check_and_replace b, check_and_replace f))
  and check_and_replace e =
    if [%eq: exp] e old_subexp then new_subexp else recurse e
  in
  check_and_replace e

let fill_holes : (string * exp) list -> exp -> exp =
 fun bindings e ->
  let rec recurse = function
    (* Main case*)
    | EHole (name, typ) ->
        (match List.Assoc.find ~equal:String.equal bindings name with
        | Some rhs -> recurse rhs
        | None -> EHole (name, typ))
    (* Other cases *)
    | EVar x -> EVar x
    | EApp (head, arg) -> EApp (recurse head, recurse arg)
    | EAbs (param, tau, body) -> EAbs (param, tau, recurse body)
    | EMatch (scrutinee, branches) ->
        EMatch (recurse scrutinee, map_branches ~f:recurse branches)
    | ECtor (ctor_name, arg) -> ECtor (ctor_name, recurse arg)
    | EPair (e1, e2) -> EPair (recurse e1, recurse e2)
    | EFst arg -> EFst (recurse arg)
    | ESnd arg -> ESnd (recurse arg)
    | EUnit -> EUnit
    | EInt n -> EInt n
    | ERScheme (RListFoldr (b, f)) ->
        ERScheme (RListFoldr (recurse b, recurse f))
  in
  recurse e

let rec clean : exp -> exp = function
  (* Main cases *)
  (* Eta-equivalence *)
  | EAbs (param, _, EApp (EVar f, EVar x)) when String.equal x param -> EVar f
  (* Other cases *)
  | EVar x -> EVar x
  | EApp (head, arg) -> EApp (clean head, clean arg)
  | EAbs (param, tau, body) -> EAbs (param, tau, clean body)
  | EMatch (scrutinee, branches) ->
      EMatch (clean scrutinee, map_branches ~f:clean branches)
  | ECtor (ctor_name, arg) -> ECtor (ctor_name, clean arg)
  | EPair (e1, e2) -> EPair (clean e1, clean e2)
  | EFst arg -> EFst (clean arg)
  | ESnd arg -> ESnd (clean arg)
  | EUnit -> EUnit
  | EInt n -> EInt n
  | EHole (name, typ) -> EHole (name, typ)
  | ERScheme (RListFoldr (b, f)) -> ERScheme (RListFoldr (clean b, clean f))
