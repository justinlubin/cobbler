open Core
open Lang

let inline : env -> exp -> exp =
 fun env e ->
  Map.fold env ~init:e ~f:(fun ~key:lhs ~data:rhs acc ->
      Lang_util.substitute (lhs, rhs) acc)

let rec fully_reduce : exp -> exp = function
  | EVar id -> EVar id
  | EApp (head, arg) ->
      let arg' = fully_reduce arg in
      (match fully_reduce head with
      | EAbs (param, body) -> Lang_util.substitute (param, arg') body
      | head' -> EApp (head', arg'))
  | EAbs (param, body) -> EAbs (param, fully_reduce body)
  | EMatch (scrutinee, branches) ->
      EMatch
        (fully_reduce scrutinee, Lang_util.map_branches ~f:fully_reduce branches)
  | ECtor (ctor_name, arg) -> ECtor (ctor_name, fully_reduce arg)
  | EInt n -> EInt n
  | EHole typ -> EHole typ

let rec pull_out_cases : exp -> exp = function
  | EVar x -> EVar x
  | EApp (head, arg) -> EApp (pull_out_cases head, pull_out_cases arg)
  | EAbs (param, body) -> EAbs (param, pull_out_cases body)
  | EMatch (outer_scrutinee, outer_branches) ->
      let outer_branches' =
        Lang_util.map_branches ~f:pull_out_cases outer_branches
      in
      (match pull_out_cases outer_scrutinee with
      | EMatch (inner_scrutinee, inner_branches) ->
          EMatch
            ( inner_scrutinee
            , Lang_util.map_branches
                ~f:(fun rhs -> EMatch (rhs, outer_branches'))
                inner_branches )
      | outer_scrutinee' -> EMatch (outer_scrutinee', outer_branches'))
  | ECtor (ctor_name, arg) -> ECtor (ctor_name, pull_out_cases arg)
  | EInt n -> EInt n
  | EHole typ -> EHole typ

let rec partially_evaluate_cases : exp -> exp = function
  | EVar x -> EVar x
  | EApp (head, arg) ->
      EApp (partially_evaluate_cases head, partially_evaluate_cases arg)
  | EAbs (param, body) -> EAbs (param, partially_evaluate_cases body)
  | EMatch (ECtor (ctor_name, arg), branches) ->
      let arg_name, rhs =
        List.Assoc.find_exn ~equal:String.equal branches ctor_name
      in
      partially_evaluate_cases (Lang_util.substitute (arg_name, arg) rhs)
  | EMatch (scrutinee, branches) ->
      EMatch
        ( partially_evaluate_cases scrutinee
        , Lang_util.map_branches ~f:partially_evaluate_cases branches )
  | ECtor (ctor_name, arg) -> ECtor (ctor_name, partially_evaluate_cases arg)
  | EInt n -> EInt n
  | EHole typ -> EHole typ

let full : env -> exp -> exp =
 fun env e ->
  e |> inline env |> fully_reduce |> pull_out_cases |> partially_evaluate_cases
