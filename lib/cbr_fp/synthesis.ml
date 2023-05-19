open Core
open Lang

(* Normalization *)

let inline : env -> exp -> exp =
 fun env e ->
  Map.fold env ~init:e ~f:(fun ~key:lhs ~data:rhs acc ->
      Exp.substitute (lhs, rhs) acc)

let norm : datatype_env -> typ_env -> env -> exp -> exp =
 fun sigma gamma env e ->
  e
  |> inline env
  |> Exp.normalize
  |> Fusion.fuse
  |> Fusion.pull_out_cases
  |> Exp.normalize

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

let expand : grammar -> int -> exp -> exp list =
 fun grammar _ e ->
  let open List.Let_syntax in
  let rec expand' = function
    | EVar x -> []
    | EApp (head, arg) ->
        (* Only does one at a time (may want to change later) *)
        List.map ~f:(fun h -> EApp (h, arg)) (expand' head)
        @ List.map ~f:(fun a -> EApp (head, a)) (expand' arg)
    | EHole (_, typ) ->
        List.map
          ~f:(fun (x, typs) ->
            Exp.build_app
              (EVar x)
              (List.map ~f:(fun typ -> EHole (Util.gensym "hole", typ)) typs))
          (Map.find grammar typ |> Option.value_or_thunk ~default:(fun _ -> []))
    | _ -> failwith "expanding something other than var, app, or hole"
  in
  expand' e

let debug_expand : grammar -> int -> exp -> exp list =
 fun grammar depth e ->
  print_endline (sprintf "{ Expanding (depth %d): %s" depth (Exp.show_single e));
  let expansion = expand grammar depth e in
  List.iter ~f:(fun e' -> print_endline ("  " ^ Exp.show_single e')) expansion;
  print_endline "}";
  expansion

(* Problems *)

type problem =
  { sigma : datatype_env
  ; gamma : typ_env
  ; env : env
  ; name : string
  }

let problem_of_definitions : datatype_env * typ_env * env -> problem =
 fun (sigma, gamma, env) ->
  { sigma
  ; gamma
  ; env =
      String.Map.mapi env ~f:(fun ~key:name ~data:old_rhs ->
          match Recursion_scheme.extract_list_foldr sigma gamma env name with
          | Some new_rhs -> new_rhs
          | None -> old_rhs)
  ; name = "main"
  }

(* Synthesis *)

let solve : use_unification:bool -> depth:int -> problem -> exp option =
 fun ~use_unification ~depth { sigma; gamma; env; name } ->
  let reference = String.Map.find_exn env "main" in
  let reference_domain, reference_codomain =
    Typ.decompose_arr (String.Map.find_exn gamma "main")
  in
  let reference_params, _ = Exp.decompose_abs reference in
  let normalized_reference = norm sigma gamma env reference in
  let normalized_reference_params, normalized_reference_body =
    Exp.decompose_abs normalized_reference
  in
  let stdlib =
    List.fold2_exn
      normalized_reference_params
      reference_domain
      ~init:gamma
      ~f:(fun acc x tau -> String.Map.update acc x ~f:(fun _ -> tau))
  in
  let normalized_reference_body_uniterm =
    Unification_adapter.to_unification_term
      sigma
      stdlib
      normalized_reference_body
  in
  let grammar =
    make_grammar
      (String.Map.remove gamma "main")
      (String.Map.remove env "main")
      (if use_unification
      then []
      else List.zip_exn reference_params reference_domain)
  in
  Option.map
    (Cbr_framework.Enumerative_search.top_down
       ~max_iterations:depth
       ~start:(EHole (Util.gensym "start", reference_codomain))
       ~expand:(expand grammar)
       ~correct:
         (if use_unification
         then
           fun candidate_body ->
           let normalized_candidate_body =
             norm sigma gamma env candidate_body
           in
           let normalized_candidate_body_uniterm =
             Unification_adapter.to_unification_term
               sigma
               stdlib
               normalized_candidate_body
           in
           match
             Unification.unify
               1000
               normalized_candidate_body_uniterm
               normalized_reference_body_uniterm
           with
           | Unification.Solved subs ->
               Some
                 (Exp.build_abs
                    normalized_reference_params
                    (Exp.fill_holes
                       (Unification_adapter.simplify_solution sigma subs)
                       candidate_body))
           | Unification.Impossible -> None
           | Unification.OutOfFuel -> None
         else
           fun candidate_body ->
           let candidate = Exp.build_abs reference_params candidate_body in
           if Exp.alpha_equivalent
                (norm sigma gamma env candidate)
                normalized_reference
           then Some candidate
           else None))
    ~f:(fun messy_solution ->
      Exp.normalize
        (Exp.build_abs
           reference_params
           (Exp.build_app
              messy_solution
              (List.map ~f:(fun x -> EVar x) reference_params))))
