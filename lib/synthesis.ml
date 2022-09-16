open Core
open Lang

(* Normalization *)

let inline : env -> exp -> exp =
 fun env e ->
  Map.fold env ~init:e ~f:(fun ~key:lhs ~data:rhs acc ->
      Exp.substitute (lhs, rhs) acc)

let norm : env -> exp -> exp =
 fun env e ->
  e
  |> inline env
  |> Exp.beta_normalize
  |> Fusion.pull_out_cases
  |> Fusion.case_normalize

(* Grammars *)

type grammar = (typ, (id * typ list) list, Typ.comparator_witness) Map.t

let make_grammar : typ_env -> env -> (id * typ) list -> grammar =
 fun gamma env free_vars ->
  Map.fold2
    gamma
    env
    ~init:
      (Map.of_alist_multi
         (module Typ)
         (List.map ~f:(fun (id, typ) -> (typ, (id, []))) free_vars))
    ~f:(fun ~key ~data acc ->
      match data with
      | `Right _ -> failwith "env contains key gamma does not"
      | `Left _ -> failwith "gamma contains key env does not"
      | `Both (typ, exp) ->
          let domain, codomain = Typ.decompose_arr typ in
          Map.add_multi acc ~key:codomain ~data:(key, domain))

(* Expansion *)

let make_app : id -> exp list -> exp =
 fun head args ->
  List.fold_left ~init:(EVar head) ~f:(fun acc x -> EApp (acc, x)) args

let expand : grammar -> exp -> exp list =
 fun grammar e ->
  let open List.Let_syntax in
  let rec expand' = function
    | EVar x -> []
    | EApp (head, arg) ->
        (* Only does one at a time (may want to change later) *)
        List.map ~f:(fun h -> EApp (h, arg)) (expand' head)
        @ List.map ~f:(fun a -> EApp (head, a)) (expand' arg)
    | EHole typ ->
        List.map
          ~f:(fun (x, typs) ->
            make_app x (List.map ~f:(fun typ -> EHole typ) typs))
          (Map.find grammar typ |> Option.value_or_thunk ~default:(fun _ -> []))
    | EAbs _ | EMatch _ | ECtor _ | EInt _ ->
        failwith "expanding something other than var, app, or hole"
  in
  expand' e

let debug_expand : grammar -> exp -> exp list =
 fun grammar e ->
  print_endline ("{ Expanding: " ^ Exp.show e);
  let expansion = expand grammar e in
  List.iter ~f:(fun e' -> print_endline ("  " ^ Exp.show e')) expansion;
  print_endline "}";
  expansion

(* Problems *)

type problem =
  { gamma : typ_env
  ; env : env
  ; free_vars : (id * typ) list
  ; goal_typ : typ
  ; reference : exp
  }

let problem_of_definitions : typ_env * env -> problem =
 fun (gamma, env) ->
  let main_typ = Map.find_exn gamma "main" in
  let main_exp = Map.find_exn env "main" in
  let _, main_codomain = Typ.decompose_arr main_typ in
  let main_params, main_body = Exp.decompose_abs main_exp in
  { gamma = Map.remove gamma "main"
  ; env = Map.remove env "main"
  ; free_vars = main_params
  ; goal_typ = main_codomain
  ; reference = main_body
  }

(* Synthesis *)

let solve : problem -> exp option =
 fun { gamma; env; free_vars; goal_typ; reference } ->
  let normalized_reference = norm env reference in
  let grammar = make_grammar gamma env free_vars in
  Enumerative_search.top_down
    ~max_iterations:5
    ~start:(EHole goal_typ)
    ~expand:(expand grammar)
    ~correct:(fun e ->
      let result = Exp.alpha_equivalent (norm env e) normalized_reference in
      result)
