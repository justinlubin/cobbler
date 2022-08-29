open Core
open Lang

(* Helpers *)

let rec decompose_arr : typ -> typ list * typ = function
  | TPlaceholder x -> ([], TPlaceholder x)
  | TArr (domain, codomain) ->
      let domain', codomain' = decompose_arr codomain in
      (domain :: domain', codomain')

let decompose_abs : exp -> id list * exp =
 fun e ->
  let rec decompose_abs' acc = function
    | EAbs (param, body) -> decompose_abs' (param :: acc) body
    | rest -> (List.rev acc, rest)
  in
  decompose_abs' [] e

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
          let domain, codomain = decompose_arr typ in
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
  print_endline ("{ Expanding: " ^ Lang_util.show_exp e);
  let expansion = expand grammar e in
  List.iter
    ~f:(fun e' -> print_endline ("  " ^ Lang_util.show_exp e'))
    expansion;
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
  let main_domain, main_codomain = decompose_arr main_typ in
  let main_params, main_body = decompose_abs main_exp in
  { gamma = Map.remove gamma "main"
  ; env = Map.remove env "main"
  ; free_vars = List.zip_exn main_params main_domain
  ; goal_typ = main_codomain
  ; reference = main_body
  }

(* Synthesis *)

let solve : problem -> exp option =
 fun { gamma; env; free_vars; goal_typ; reference } ->
  let normalized_reference = Normalize.full env reference in
  let grammar = make_grammar gamma env free_vars in
  Enumerative_search.top_down
    ~max_iterations:5
    ~start:(EHole goal_typ)
    ~expand:(expand grammar)
    ~correct:(fun e ->
      let result =
        Lang_util.alpha_equivalent (Normalize.full env e) normalized_reference
      in
      result)
