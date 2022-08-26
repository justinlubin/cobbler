open Core
open Lang

let rec closed_substitute : env -> exp -> exp =
 fun env -> function
  | EVar x ->
      (match Map.find env x with
      | None -> EVar x
      | Some e -> closed_substitute env e)
  | EApp (head, arg) ->
      EApp (closed_substitute env head, closed_substitute env arg)
  | EAbs (param, body) ->
      EAbs (param, closed_substitute (Map.remove env param) body)
  | EMatch (scrutinee, branches) ->
      EMatch
        ( closed_substitute env scrutinee
        , List.map
            ~f:(fun (ctor_name, (arg_name, rhs)) ->
              ( ctor_name
              , (arg_name, closed_substitute (Map.remove env arg_name) rhs) ))
            branches )
  | ECtor (ctor_name, arg) -> ECtor (ctor_name, closed_substitute env arg)

let map_branches : branch list -> f:(exp -> exp) -> branch list =
 fun branches ~f ->
  List.map branches ~f:(fun (ctor_name, (arg_name, rhs)) ->
      (ctor_name, (arg_name, f rhs)))
