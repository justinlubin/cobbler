open Core
open Lang
module T = Util.Timing_breakdown

(* Normalization *)

let inline : env -> exp -> exp =
 fun env e ->
  Map.fold env ~init:e ~f:(fun ~key:lhs ~data:rhs acc ->
      if String.is_suffix ~suffix:"____CBR_builtin" lhs
      then acc
      else Exp.substitute (lhs, rhs) acc)

let norm : datatype_env -> typ_env -> env -> exp -> exp =
 fun sigma gamma env e ->
  e
  |> inline env
  |> Exp.normalize sigma
  |> Fusion.fuse sigma
  |> Exp.normalize sigma

(* Grammar and expansion *)

type grammar = (string * typ_scheme) list

let make_grammar : typ_env -> (string * typ) list -> grammar =
 fun gamma free_vars ->
  Map.to_alist gamma @ List.map ~f:(fun (x, tau) -> (x, ([], tau))) free_vars

let expand' : grammar -> exp -> (Typ.sub * exp) list =
 fun g e ->
  let rec recur = function
    | EVar x -> []
    | EApp (head, arg) ->
        (* Only does one at a time (may want to change later) *)
        List.map
          ~f:(fun (subst, head') ->
            (subst, EApp (head', Exp.apply_type_sub subst arg)))
          (recur head)
        @ List.map
            ~f:(fun (subst, arg') ->
              (subst, EApp (Exp.apply_type_sub subst head, arg')))
            (recur arg)
    | EHole (_, t) ->
        List.filter_map
          ~f:(fun (f, ts_f) ->
            let t_f = Typ.instantiate ts_f in
            let domain_f, codomain_f = Typ.decompose_arr t_f in
            match Type_system.unify [ (t, codomain_f) ] with
            | Some subst ->
                Some
                  ( subst
                  , Exp.build_app
                      (EVar f)
                      (List.map
                         ~f:(fun dom ->
                           EHole (Util.gensym "hole", Typ.apply_sub subst dom))
                         domain_f) )
            | None -> None)
          g
    | _ -> failwith "expanding something other than var, app, or hole"
  in
  recur e

let expand : grammar -> int -> exp -> exp list =
 fun g _depth e -> List.map ~f:snd (expand' g e)

let debug_expand : grammar -> int -> exp -> exp list =
 fun grammar depth e ->
  Printf.eprintf "{ Expanding (depth %d): %s\n" depth (Exp.show_single e);
  let expansions = expand grammar depth e in
  List.iter ~f:(fun e' -> print_endline ("  " ^ Exp.show_single e')) expansions;
  print_endline "}";
  expansions

(* Problems *)

type problem =
  { sigma : datatype_env
  ; gamma : typ_env
  ; env : env
  ; name : string
  }

let problem_of_definitions : datatype_env * typ_env * env -> string -> problem =
 fun (sigma, gamma, env) name ->
  { sigma
  ; gamma
  ; env =
      Map.mapi env ~f:(fun ~key:name ~data:old_rhs ->
          Recursion_scheme.rewrite sigma name old_rhs)
  ; name
  }

(* Synthesis *)

let solve
    :  use_semantic_unification:bool -> depth:int -> problem
    -> (int * exp) option
  =
 fun ~use_semantic_unification ~depth { sigma; gamma; env; name } ->
  let reference = Map.find_exn env name in
  let reference_domain, reference_codomain =
    Typ.decompose_arr (Typ.instantiate (Map.find_exn gamma name))
  in
  let reference_params, _ = Exp.decompose_abs reference in
  let normalized_reference =
    T.record_thunk T.CanonicalizationOutsideEnumeration (fun () ->
        norm sigma gamma env reference)
  in
  let normalized_reference_params, normalized_reference_body =
    Exp.decompose_abs normalized_reference
  in
  let stdlib =
    List.fold2_exn
      normalized_reference_params
      reference_domain
      ~init:gamma
      ~f:(fun acc x tau -> Map.update acc x ~f:(fun _ -> ([], tau)))
  in
  let grammar = make_grammar (Map.remove gamma name) [] in
  let canon = T.record4 T.CanonicalizationInsideEnumeration norm in
  Option.map
    (T.record_thunk T.Enumeration (fun () ->
         Cbr_framework.Enumerative_search.top_down
           ~max_iterations:depth
           ~start:[ EHole (Util.gensym "start", reference_codomain) ]
           ~expand:(expand grammar)
           ~correct:
             (if use_semantic_unification
             then (
               let normalized_reference_body_uniterm =
                 T.record_thunk T.UnificationInsideEnumeration (fun () ->
                     Unification_adapter.to_unification_term
                       sigma
                       stdlib
                       normalized_reference_body)
               in
               let unify =
                 T.record1
                   T.UnificationInsideEnumeration
                   (fun normalized_candidate_body ->
                     let normalized_candidate_body_uniterm =
                       Unification_adapter.to_unification_term
                         sigma
                         stdlib
                         normalized_candidate_body
                     in
                     Unification.unify
                       1000
                       normalized_candidate_body_uniterm
                       normalized_reference_body_uniterm)
               in
               fun candidate_body ->
                 let normalized_candidate_body =
                   canon sigma gamma env candidate_body
                 in
                 match unify normalized_candidate_body with
                 | Unification.Solved subs ->
                     Some
                       (Exp.build_abs
                          normalized_reference_params
                          (Exp.fill_holes
                             (Unification_adapter.simplify_solution sigma subs)
                             candidate_body))
                 | Unification.Impossible -> None
                 | Unification.OutOfFuel -> None)
             else (
               let normalized_reference_body =
                 T.record_thunk T.UnificationInsideEnumeration (fun () ->
                     Exp.alpha_normalize normalized_reference_body)
               in
               let unify =
                 T.record1
                   T.UnificationInsideEnumeration
                   (fun normalized_candidate_body ->
                     Exp.pattern_match
                       ~reference:normalized_reference_body
                       ~sketch:normalized_candidate_body)
               in
               fun candidate_body ->
                 let normalized_candidate_body =
                   canon sigma gamma env candidate_body
                 in
                 Option.map
                   ~f:(fun filling ->
                     candidate_body
                     |> Exp.fill_holes filling
                     |> Exp.build_abs normalized_reference_params)
                   (unify normalized_candidate_body)))))
    ~f:(fun (expansions, messy_solution) ->
      ( expansions
      , Exp.normalize
          sigma
          (Exp.build_abs
             reference_params
             (Exp.build_app
                messy_solution
                (List.map ~f:(fun x -> EVar x) reference_params))) ))
