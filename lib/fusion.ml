open Core
open Lang

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

let rec fully_case_reduce : exp -> exp = function
  | EVar x -> EVar x
  | EApp (head, arg) -> EApp (fully_case_reduce head, fully_case_reduce arg)
  | EAbs (param, body) -> EAbs (param, fully_case_reduce body)
  | EMatch (ECtor (ctor_name, arg), branches) ->
      let arg_name, rhs =
        List.Assoc.find_exn ~equal:String.equal branches ctor_name
      in
      fully_case_reduce (Lang_util.substitute (arg_name, arg) rhs)
  | EMatch (scrutinee, branches) ->
      EMatch
        ( fully_case_reduce scrutinee
        , Lang_util.map_branches ~f:fully_case_reduce branches )
  | ECtor (ctor_name, arg) -> ECtor (ctor_name, fully_case_reduce arg)
  | EInt n -> EInt n
  | EHole typ -> EHole typ
