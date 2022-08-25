open Core
open Lang

let inline : env -> exp -> exp =
 fun lib e ->
  let rec inline' shadows = function
    | EVar x -> if Set.mem shadows x then EVar x else Map.find_exn lib x
    | EApp (e1, e2) -> EApp (inline' shadows e1, inline' shadows e2)
    | EAbs (x, body) -> EAbs (x, inline' (Set.add shadows x) body)
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

let substitute : string * exp -> exp -> exp =
 fun (x, e) -> inline (Map.singleton (module String) x e)

let rec partially_evaluate_cases : exp -> exp = function
  | EVar x -> EVar x
  | EApp (e1, e2) ->
      EApp (partially_evaluate_cases e1, partially_evaluate_cases e2)
  | EAbs (x, body) -> EAbs (x, partially_evaluate_cases body)
  | EMatch (ECtor (ctor_name, arg), branches) ->
      let arg_name, rhs =
        List.Assoc.find_exn ~equal:String.equal branches ctor_name
      in
      partially_evaluate_cases (substitute (arg_name, arg) rhs)
  | EMatch (scrutinee, branches) ->
      EMatch
        ( partially_evaluate_cases scrutinee
        , List.map
            ~f:(fun (ctor_name, (arg_name, rhs)) ->
              (ctor_name, (arg_name, partially_evaluate_cases rhs)))
            branches )
  | ECtor (ctor_name, arg) -> ECtor (ctor_name, partially_evaluate_cases arg)

let pull_out_cases : exp -> exp = fun e -> failwith "TODO"
let full : env -> exp -> exp = fun lib e -> failwith "TODO"
