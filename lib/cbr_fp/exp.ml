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
              ~f:(fun (ctor_name, (arg_names, rhs)) ->
                sprintf
                  "((%s%s) -> %s)"
                  ctor_name
                  (List.map ~f:(fun a -> " " ^ a) arg_names |> String.concat)
                  (show_single rhs))
              branches))
  | ECtor (ctor_name, args) ->
      sprintf
        "(%s%s)"
        ctor_name
        (List.map ~f:(fun a -> " " ^ show_single a) args |> String.concat)
  | EBase (BEInt n) -> string_of_int n
  | EBase (BEFloat f) -> string_of_float f
  | EBase (BEString s) -> sprintf "\"%s\"" s
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
              ~f:(fun (ctor_name, (arg_names, rhs)) ->
                sprintf
                  "%s%s((%s%s) ->\n%s)"
                  indent
                  single_indent
                  ctor_name
                  (List.map ~f:(fun a -> " " ^ a) arg_names |> String.concat)
                  (show_multi (depth + 2) rhs))
              branches))
  | ECtor (ctor_name, args) ->
      sprintf
        "(%s%s)"
        ctor_name
        (List.map ~f:(fun a -> " " ^ show_single a) args |> String.concat)
  | EBase (BEInt n) -> string_of_int n
  | EBase (BEFloat f) -> string_of_float f
  | EBase (BEString s) -> sprintf "\"%s\"" s
  | EHole (name, typ) -> sprintf "(?? %s %s)" name (Typ.show typ)
  | ERScheme (RListFoldr (b, f)) ->
      sprintf "(list_foldr %s %s)" (show_single b) (show_single f)

let map_branches : branch list -> f:(exp -> exp) -> branch list =
 fun branches ~f ->
  List.map branches ~f:(fun (ctor_name, (arg_name, rhs)) ->
      (ctor_name, (arg_name, f rhs)))

let decompose_abs : exp -> (string * typ) list * exp =
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

let build_abs : (string * typ) list -> exp -> exp =
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
             ~f:(fun (_, (arg_names, rhs)) ->
               Set.diff (free_variables rhs) (String.Set.of_list arg_names))
             branches)
  | ECtor (_, arg) -> String.Set.union_list (List.map ~f:free_variables arg)
  | ERScheme (RListFoldr (b, f)) ->
      String.Set.union (free_variables b) (free_variables f)
  | EBase _ | EHole (_, _) -> String.Set.empty

let replace : string * string -> exp -> exp =
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
          , List.map branches ~f:(fun (ctor_name, (arg_names, branch_rhs)) ->
                ( ctor_name
                , ( List.map
                      ~f:(fun arg_name ->
                        if String.equal lhs arg_name then rhs else arg_name)
                      arg_names
                  , replace' branch_rhs ) )) )
    | ECtor (ctor_name, args) -> ECtor (ctor_name, List.map ~f:replace' args)
    | EBase b -> EBase b
    | EHole (name, typ) -> EHole (name, typ)
    | ERScheme (RListFoldr (b, f)) ->
        ERScheme (RListFoldr (replace' b, replace' f))
  in
  replace' e

let replace_all : (string * string) list -> exp -> exp =
 fun subs e -> List.fold_left ~f:(fun acc sub -> replace sub acc) ~init:e subs

let gensym_prefix : string = "var"

let substitute : string * exp -> exp -> exp =
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
              ~f:(fun (ctor_name, (arg_names, branch_rhs)) ->
                if List.mem ~equal:[%eq: string] arg_names lhs
                then (ctor_name, (arg_names, branch_rhs))
                else if List.for_all arg_names ~f:(fun arg_name ->
                            not (Set.mem rhs_fv arg_name))
                then (ctor_name, (arg_names, substitute' branch_rhs))
                else (
                  let replacements =
                    List.map
                      ~f:(fun a -> (a, Util.gensym gensym_prefix))
                      arg_names
                  in
                  ( ctor_name
                  , ( List.map ~f:snd replacements
                    , substitute' (replace_all replacements branch_rhs) ) )))
              branches )
    | ECtor (ctor_name, args) -> ECtor (ctor_name, List.map ~f:substitute' args)
    | EBase b -> EBase b
    | EHole (name, typ) -> EHole (name, typ)
    | ERScheme (RListFoldr (b, f)) ->
        ERScheme (RListFoldr (substitute' b, substitute' f))
  in
  substitute' e

let substitute_all : (string * exp) list -> exp -> exp =
 fun subs e ->
  List.fold_left ~f:(fun acc sub -> substitute sub acc) ~init:e subs

let freshen_with : (string -> string) -> exp -> exp =
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
          , List.map branches ~f:(fun (ctor_name, (arg_names, rhs)) ->
                let replacements =
                  List.map ~f:(fun a -> (a, renamer a)) arg_names
                in
                ( ctor_name
                , ( List.map ~f:snd replacements
                  , freshen_with' (replace_all replacements rhs) ) )) )
    | ECtor (ctor_name, args) ->
        ECtor (ctor_name, List.map ~f:freshen_with' args)
    | EBase b -> EBase b
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
      | ERScheme (RListFoldr (b, f)), ECtor ("Nil", []) -> b
      | ERScheme (RListFoldr (b, f)), ECtor ("Cons", [ hd; tl ]) ->
          normalize
            (EApp (EApp (f, hd), EApp (ERScheme (RListFoldr (b, f)), tl)))
      | head', arg' -> EApp (head', arg'))
  | EAbs (param, tau, body) -> EAbs (param, tau, normalize body)
  | EMatch (scrutinee, branches) ->
      (match normalize scrutinee with
      | ECtor (ctor_name, args) ->
          let arg_names, rhs =
            List.Assoc.find_exn ~equal:String.equal branches ctor_name
          in
          normalize (substitute_all (List.zip_exn arg_names args) rhs)
      | scrutinee' -> EMatch (scrutinee', map_branches ~f:normalize branches))
  | ECtor (ctor_name, args) -> ECtor (ctor_name, List.map ~f:normalize args)
  | EBase b -> EBase b
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
    | ECtor (ctor_name, args) ->
        ECtor (ctor_name, List.map ~f:check_and_replace args)
    | EBase b -> EBase b
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
    | ECtor (ctor_name, arg) -> ECtor (ctor_name, List.map ~f:recurse arg)
    | EBase b -> EBase b
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
  | ECtor (ctor_name, arg) -> ECtor (ctor_name, List.map ~f:clean arg)
  | EBase b -> EBase b
  | EHole (name, typ) -> EHole (name, typ)
  | ERScheme (RListFoldr (b, f)) -> ERScheme (RListFoldr (clean b, clean f))
