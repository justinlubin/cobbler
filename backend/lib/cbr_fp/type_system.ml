open Core
open Lang

(* This module implements type inference via HM-style constraint generation
   and checking. See, for example, TAPL Chapter 22. *)

(* Type constraints *)

type constraint_set = (typ * typ) list

let apply_sub_constraints : Typ.sub -> constraint_set -> constraint_set =
 fun sigma cs ->
  List.map ~f:(fun (s, t) -> (Typ.apply_sub sigma s, Typ.apply_sub sigma t)) cs

(* Constraint typing *)

exception IllTyped of exp [@@deriving sexp]

let polymorphic_variant_universe : string = "POLYMORPHIC_VARIANT____CBR"

let rec constraint_type : datatype_env -> typ_env -> exp -> typ * constraint_set
  =
 fun sigma gamma e ->
  match e with
  | EVar x ->
      (match Map.find gamma x with
      | None -> raise (IllTyped e)
      | Some t -> (Typ.instantiate t, []))
  | EApp (e1, e2) ->
      let t1, c1 = constraint_type sigma gamma e1 in
      let t2, c2 = constraint_type sigma gamma e2 in
      let x = Typ.fresh_type_var () in
      (x, ((t1, TArr (t2, x)) :: c1) @ c2)
  | EAbs (param, body) ->
      let x = Typ.fresh_type_var () in
      let t2, c =
        constraint_type
          sigma
          (Map.update gamma param ~f:(fun _ -> ([], x)))
          body
      in
      (TArr (x, t2), c)
  | EMatch (scrutinee, branches) ->
      (match branches with
      | [] -> raise (IllTyped e)
      | (first_ctor, _) :: _ ->
          (match Typ.ctor_typ sigma first_ctor with
          | Some ((dt, dt_params), _) ->
              let dt_sub_list =
                List.map ~f:(fun p -> (p, Typ.fresh_type_var ())) dt_params
              in
              let dt_sub = String.Map.of_alist_exn dt_sub_list in
              let ctors, rhs_types, rhs_constraints =
                List.unzip3
                  (List.map branches ~f:(fun (tag, (arg_names, rhs)) ->
                       match Typ.ctor_typ sigma tag with
                       | Some (_, domains) ->
                           let t_rhs, c_rhs =
                             constraint_type
                               sigma
                               (List.fold2_exn
                                  arg_names
                                  domains
                                  ~init:gamma
                                  ~f:(fun acc a d ->
                                    Map.update acc a ~f:(fun _ ->
                                        ([], Typ.apply_sub dt_sub d))))
                               rhs
                           in
                           (tag, t_rhs, c_rhs)
                       | None -> raise (IllTyped e)))
              in
              if List.equal
                   [%eq: string]
                   (List.sort ctors ~compare:[%compare: string])
                   (List.sort
                      (List.map ~f:fst (snd (Map.find_exn sigma dt)))
                      ~compare:[%compare: string])
              then (
                let t_scrutinee, c_scrutinee =
                  constraint_type sigma gamma scrutinee
                in
                let return_type = Typ.fresh_type_var () in
                ( return_type
                , ((t_scrutinee, TDatatype (dt, List.map ~f:snd dt_sub_list))
                  :: c_scrutinee)
                  @ List.map ~f:(fun t -> (return_type, t)) rhs_types
                  @ List.concat rhs_constraints ))
              else raise (IllTyped e)
          | None -> raise (IllTyped e)))
  | ECtor (tag, args) ->
      let ts_args, cs_args =
        List.unzip (List.map ~f:(constraint_type sigma gamma) args)
      in
      (match Typ.ctor_typ sigma tag with
      | Some ((dt, params), domains) ->
          let sub_list =
            List.map ~f:(fun p -> (p, Typ.fresh_type_var ())) params
          in
          let sub = String.Map.of_alist_exn sub_list in
          ( TDatatype (dt, List.map ~f:snd sub_list)
          , List.map2_exn domains ts_args ~f:(fun d t ->
                (Typ.apply_sub sub d, t))
            @ List.concat cs_args )
      | None ->
          (* raise (IllTyped e)) *)
          (TDatatype (polymorphic_variant_universe, []), []))
  | EBase (BEInt _) -> (TBase BTInt, [])
  | EBase (BEFloat _) -> (TBase BTFloat, [])
  | EBase (BEString _) -> (TBase BTString, [])
  | EHole (_, t) -> (t, [])
  | ERScheme (RSCata, dt, args) ->
      let dt_params, ctors = Map.find_exn sigma dt in
      let sub_list =
        List.map ~f:(fun p -> (p, Typ.fresh_type_var ())) dt_params
      in
      let sub = String.Map.of_alist_exn sub_list in
      let return_type_var = Typ.fresh_type_var () in
      let args_constraints =
        try
          List.map2_exn args ctors ~f:(fun arg (_, domain) ->
              let t_arg, c_arg = constraint_type sigma gamma arg in
              ( t_arg
              , Typ.build_arr
                  (List.map
                     ~f:(fun d ->
                       match d with
                       | TDatatype (dt', _) when String.equal dt dt' ->
                           return_type_var
                       | _ -> Typ.apply_sub sub d)
                     domain)
                  return_type_var )
              :: c_arg)
        with
        | _ -> failwith ([%show: exp list] args)
      in
      ( TArr (TDatatype (dt, List.map ~f:snd sub_list), return_type_var)
      , List.concat args_constraints )

(* Constraint unification *)

exception CannotUnify of (typ * typ) list [@@deriving sexp]

let rec unify_exn : constraint_set -> Typ.sub =
 fun cs ->
  match cs with
  | [] -> String.Map.empty
  | (s, t) :: tail ->
      let fvs = Typ.free_vars s in
      let fvt = Typ.free_vars t in
      (match (s, t) with
      (* Base cases *)
      | TBase b1, TBase b2 when [%eq: base_typ] b1 b2 -> unify_exn tail
      | TVar x1, TVar x2 when String.equal x1 x2 -> unify_exn tail
      | TDatatype (dt1, _), TDatatype (dt2, _)
        when String.equal dt1 polymorphic_variant_universe
             || String.equal dt2 polymorphic_variant_universe -> unify_exn tail
      (* Free variable cases *)
      | TVar x, _ when not (Set.mem fvt x) ->
          let sub = String.Map.singleton x t in
          Typ.compose_subs (unify_exn (apply_sub_constraints sub tail)) sub
      | _, TVar x when not (Set.mem fvs x) ->
          let sub = String.Map.singleton x s in
          Typ.compose_subs (unify_exn (apply_sub_constraints sub tail)) sub
      (* Recursive cases*)
      | TDatatype (dt1, args1), TDatatype (dt2, args2) when String.equal dt1 dt2
        ->
          (match List.map2 ~f:(fun a1 a2 -> (a1, a2)) args1 args2 with
          | List.Or_unequal_lengths.Unequal_lengths -> raise (CannotUnify cs)
          | List.Or_unequal_lengths.Ok arg_constraints ->
              unify_exn (arg_constraints @ tail))
      | TArr (dom1, cod1), TArr (dom2, cod2) ->
          unify_exn ((dom1, dom2) :: (cod1, cod2) :: tail)
      (* Failure cases *)
      | TBase _, _ | TVar _, _ | TDatatype (_, _), _ | TArr (_, _), _ ->
          raise (CannotUnify cs))

let unify : (typ * typ) list -> Typ.sub option =
 fun constraints ->
  try Some (unify_exn constraints) with
  | CannotUnify _ -> None

(* Type system interface *)

let infer : datatype_env -> typ_env -> exp -> typ =
 fun sigma gamma e ->
  let s, c = constraint_type sigma gamma e in
  let sub = unify_exn c in
  Typ.apply_sub sub s

let check : datatype_env -> typ_env -> exp -> typ -> unit =
 fun sigma gamma e tau ->
  let _ = unify_exn [ (infer sigma gamma e, tau) ] in
  ()

let well_typed : datatype_env * typ_env * env -> unit =
 fun (sigma, gamma, env) ->
  Map.iteri env ~f:(fun ~key:name ~data:body ->
      check
        sigma
        gamma
        body
        (name
        |> Map.find gamma
        |> Option.value_or_thunk ~default:(fun _ -> raise (IllTyped body))
        |> Typ.instantiate))
