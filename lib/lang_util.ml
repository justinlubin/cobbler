open Core
open Lang

let rec show_exp : exp -> string = function
  | EVar id -> id
  | EApp (head, arg) -> sprintf "(%s %s)" (show_exp head) (show_exp arg)
  | EAbs (param, body) -> sprintf "(lambda %s %s)" param (show_exp body)
  | EMatch (scrutinee, branches) ->
      sprintf
        "(match %s %s)"
        (show_exp scrutinee)
        (String.concat
           ~sep:" "
           (List.map
              ~f:(fun (ctor_name, (arg_name, rhs)) ->
                sprintf "(%s %s -> %s)" ctor_name arg_name (show_exp rhs))
              branches))
  | ECtor (ctor_name, arg) -> sprintf "(%s %s)" ctor_name (show_exp arg)
  | EInt n -> string_of_int n

let map_branches : branch list -> f:(exp -> exp) -> branch list =
 fun branches ~f ->
  List.map branches ~f:(fun (ctor_name, (arg_name, rhs)) ->
      (ctor_name, (arg_name, f rhs)))

(* (lambda z. z ) (x, y) -> [z/(x, y)] z -> [x/(x, y)] (x, y) -> ([x/(x, y)]x, [x/(x, y)]y)... *)

let rec free_variables : exp -> (id, String.comparator_witness) Set.t = function
  | EVar x -> Set.singleton (module String) x
  | EApp (head, arg) -> Set.union (free_variables head) (free_variables arg)
  | EAbs (param, body) -> Set.remove (free_variables body) param
  | EMatch (scrutinee, branches) ->
      Set.union_list
        (module String)
        (free_variables scrutinee
        :: List.map
             ~f:(fun (_, (arg_name, rhs)) ->
               Set.remove (free_variables rhs) arg_name)
             branches)
  | ECtor (_, arg) -> free_variables arg
  | EInt n -> Set.empty (module String)

let suffix : int ref = ref (-1)

let gensym : unit -> string =
 fun () ->
  suffix := !suffix + 1;
  sprintf "__var%i" !suffix

let replace : id * id -> exp -> exp =
 fun (lhs, rhs) e ->
  let rec replace' = function
    | EVar x -> if String.equal lhs x then EVar rhs else EVar x
    | EApp (head, arg) -> EApp (replace' head, replace' arg)
    | EAbs (param, body) ->
        if String.equal lhs param
        then EAbs (rhs, replace' body)
        else EAbs (param, replace' body)
    | EMatch (scrutinee, branches) ->
        EMatch
          ( replace' scrutinee
          , List.map branches ~f:(fun (ctor_name, (arg_name, branch_rhs)) ->
                if String.equal lhs arg_name
                then (ctor_name, (rhs, replace' branch_rhs))
                else (ctor_name, (arg_name, replace' branch_rhs))) )
    | ECtor (ctor_name, arg) -> ECtor (ctor_name, replace' arg)
    | EInt n -> EInt n
  in
  replace' e

let substitute : id * exp -> exp -> exp =
 fun (lhs, rhs) e ->
  let rhs_fv = free_variables rhs in
  let rec substitute' = function
    | EVar x -> if String.equal lhs x then rhs else EVar x
    | EApp (head, arg) -> EApp (substitute' head, substitute' arg)
    | EAbs (param, body) ->
        if String.equal lhs param
        then EAbs (param, body)
        else if not (Set.mem rhs_fv param)
        then EAbs (param, substitute' body)
        else (
          let new_param = gensym () in
          EAbs (new_param, substitute' (replace (param, new_param) body)))
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
                  let new_arg_name = gensym () in
                  ( ctor_name
                  , ( new_arg_name
                    , substitute' (replace (arg_name, new_arg_name) branch_rhs)
                    ) )))
              branches )
    | ECtor (ctor_name, arg) -> ECtor (ctor_name, substitute' arg)
    | EInt n -> EInt n
  in
  substitute' e

let freshen_exp : (id -> id) -> exp -> exp =
 fun renamer e ->
  let rec freshen_exp' = function
    | EVar x -> EVar x
    | EApp (head, arg) -> EApp (freshen_exp' head, freshen_exp' arg)
    | EAbs (param, body) ->
        let new_param = renamer param in
        EAbs (new_param, freshen_exp' (replace (param, new_param) body))
    | EMatch (scrutinee, branches) ->
        EMatch
          ( freshen_exp' scrutinee
          , List.map branches ~f:(fun (ctor_name, (arg_name, rhs)) ->
                let new_arg_name = renamer arg_name in
                ( ctor_name
                , ( new_arg_name
                  , freshen_exp' (replace (arg_name, new_arg_name) rhs) ) )) )
    | ECtor (ctor_name, arg) -> ECtor (ctor_name, freshen_exp' arg)
    | EInt n -> EInt n
  in
  freshen_exp' e

(* let freshen : env -> env =
 fun env ->
  let suffix = ref (-1) in
  Map.map
    env
    ~f:
      (freshen_exp (fun x ->
           suffix := !suffix + 1;
           x ^ Int.to_string !suffix)) *)

let alpha_normalize : exp -> exp =
 fun e ->
  let suffix = ref (-1) in
  freshen_exp
    (fun _ ->
      suffix := !suffix + 1;
      "var" ^ Int.to_string !suffix)
    e

let alpha_equivalent : exp -> exp -> bool =
 fun e1 e2 -> [%eq: exp] (alpha_normalize e1) (alpha_normalize e2)
