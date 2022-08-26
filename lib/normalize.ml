open Core
open Lang

(* [substitute subs e] assumes [e] is closed (except for the bindings in
   [subs]). *)
let substitute : (id * exp) list -> exp -> exp =
 fun subs e ->
  let rec substitute' shadows = function
    | EVar x ->
        if Set.mem shadows x
        then EVar x
        else List.Assoc.find_exn ~equal:String.equal subs x
    | EApp (head, args) ->
        EApp (substitute' shadows head, List.map ~f:(substitute' shadows) args)
    | EAbs (params, body) ->
        EAbs
          ( params
          , substitute'
              (Set.union shadows (Set.of_list (module String) params))
              body )
    | EMatch (scrutinee, branches) ->
        EMatch
          ( substitute' shadows scrutinee
          , List.map
              ~f:(fun (ctor_name, (arg_name, rhs)) ->
                ( ctor_name
                , (arg_name, substitute' (Set.add shadows arg_name) rhs) ))
              branches )
    | ECtor (ctor_name, arg) -> ECtor (ctor_name, substitute' shadows arg)
  in
  substitute' (Set.empty (module String)) e

let inline : library -> exp -> exp =
 fun lib e ->
  let rec inline' shadows = function
    | EVar x ->
        if Set.mem shadows x
        then EVar x
        else (
          let params, rhs = Map.find_exn lib x in
          if List.is_empty params
          then inline' shadows rhs
          else
            EAbs
              ( params
              , inline'
                  (Set.union shadows (Set.of_list (module String) params))
                  rhs ))
    | EApp (EVar f, args) ->
        if Set.mem shadows f
        then EApp (EVar f, List.map ~f:(inline' shadows) args)
        else (
          let params, rhs = Map.find_exn lib f in
          if List.is_empty params
          then EApp (inline' shadows rhs, List.map ~f:(inline' shadows) args)
          else
            inline'
              (Set.union shadows (Set.of_list (module String) params))
              (substitute (List.zip_exn params args) rhs))
    | EApp (head, args) ->
        EApp (inline' shadows head, List.map ~f:(inline' shadows) args)
    | EAbs (params, body) ->
        EAbs
          ( params
          , inline'
              (Set.union shadows (Set.of_list (module String) params))
              body )
    | EMatch (scrut, branches) ->
        EMatch
          ( inline' shadows scrut
          , List.map
              ~f:(fun (ctor_name, (arg_name, rhs)) ->
                (ctor_name, (arg_name, inline' (Set.add shadows arg_name) rhs)))
              branches )
    | ECtor (ctor_name, arg) -> ECtor (ctor_name, inline' shadows arg)
  in
  inline' (Set.empty (module String)) e

let rec pull_out_cases : exp -> exp = function
  | EVar x -> EVar x
  | EApp (head, args) ->
      EApp (pull_out_cases head, List.map ~f:pull_out_cases args)
  | EAbs (params, body) -> EAbs (params, pull_out_cases body)
  | EMatch (scrutinee, outer_branches) ->
      (match pull_out_cases scrutinee with
      | EMatch (inner_scrutinee, inner_branches) ->
          EMatch
            ( inner_scrutinee
            , List.map
                ~f:(fun (ctor_name, (arg_name, rhs)) ->
                  (ctor_name, (arg_name, EMatch (rhs, outer_branches))))
                inner_branches )
      | scrutinee' ->
          EMatch
            ( scrutinee'
            , List.map
                ~f:(fun (ctor_name, (arg_name, rhs)) ->
                  (ctor_name, (arg_name, pull_out_cases rhs)))
                outer_branches ))
  | ECtor (ctor_name, arg) -> ECtor (ctor_name, pull_out_cases arg)

let rec partially_evaluate_cases : exp -> exp = function
  | EVar x -> EVar x
  | EApp (head, args) ->
      EApp
        ( partially_evaluate_cases head
        , List.map ~f:partially_evaluate_cases args )
  | EAbs (params, body) -> EAbs (params, partially_evaluate_cases body)
  | EMatch (ECtor (ctor_name, arg), branches) ->
      let arg_name, rhs =
        List.Assoc.find_exn ~equal:String.equal branches ctor_name
      in
      partially_evaluate_cases (substitute [ (arg_name, arg) ] rhs)
  | EMatch (scrutinee, branches) ->
      EMatch
        ( partially_evaluate_cases scrutinee
        , List.map
            ~f:(fun (ctor_name, (arg_name, rhs)) ->
              (ctor_name, (arg_name, partially_evaluate_cases rhs)))
            branches )
  | ECtor (ctor_name, arg) -> ECtor (ctor_name, partially_evaluate_cases arg)

let full : library -> exp -> exp =
 fun lib e -> e |> inline lib |> pull_out_cases |> partially_evaluate_cases
