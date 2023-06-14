open Core
open Lang

let nonrecursive_matches_to_catas : datatype_env -> exp -> exp =
 fun sigma e ->
  let rec recur = function
    (* Main case *)
    | EMatch (scrutinee, branches) ->
        let first_ctor, _ = List.hd_exn branches in
        let (dt, _), _ = Option.value_exn (Typ.ctor_typ sigma first_ctor) in
        let _, ctors = Map.find_exn sigma dt in
        (match
           ctors
           |> List.map ~f:(fun (ctor_name, domain) ->
                  let branch_params, rhs =
                    List.Assoc.find_exn ~equal:String.equal branches ctor_name
                  in
                  List.fold_right
                    ~f:(fun (branch_param, branch_param_type) acc ->
                      match (acc, branch_param_type) with
                      | None, _ -> None
                      | _, TDatatype (dt', _) when String.equal dt dt' -> None
                      | Some acc', _ -> Some (EAbs (branch_param, acc')))
                    ~init:(Some rhs)
                    (List.zip_exn branch_params domain))
           |> Option.all
         with
        | Some cata_args ->
            recur (EApp (ERScheme (RSCata, dt, cata_args), scrutinee))
        | None -> EMatch (recur scrutinee, Exp.map_branches ~f:recur branches))
    (* Other cases *)
    | EVar x -> EVar x
    | EApp (e1, e2) -> EApp (recur e1, recur e2)
    | EAbs (x, body) -> EAbs (x, recur body)
    | ECtor (ctor, args) -> ECtor (ctor, List.map ~f:recur args)
    | EBase b -> EBase b
    | EHole (name, tau) -> EHole (name, tau)
    | ERScheme (rs, dt, args) -> ERScheme (rs, dt, List.map ~f:recur args)
  in
  recur e

let cata_of_definition : datatype_env -> env -> string -> exp option =
 fun sigma env name ->
  match Exp.decompose_abs (String.Map.find_exn env name) with
  | top_params, EMatch (scrutinee, branches) ->
      let first_ctor, _ = List.hd_exn branches in
      let (dt, _), _ = Option.value_exn (Typ.ctor_typ sigma first_ctor) in
      let _, ctors = Map.find_exn sigma dt in
      let rs =
        ERScheme
          ( RSCata
          , dt
          , List.map ctors ~f:(fun (ctor_name, domain) ->
                let branch_params, rhs =
                  List.Assoc.find_exn ~equal:String.equal branches ctor_name
                in
                let cata_arg =
                  List.fold_right
                    ~f:(fun (branch_param, branch_param_type) acc ->
                      match branch_param_type with
                      | TDatatype (dt', _) when String.equal dt dt' ->
                          EAbs
                            ( branch_param
                            , Exp.replace_subexp
                                ~old_subexp:
                                  (Exp.build_app
                                     (EVar name)
                                     (List.drop_last_exn
                                        (List.map
                                           ~f:(fun x -> EVar x)
                                           top_params)
                                     @ [ EVar branch_param ]))
                                ~new_subexp:(EVar branch_param)
                                acc )
                      | _ -> EAbs (branch_param, acc))
                    ~init:rhs
                    (List.zip_exn branch_params domain)
                in
                if String.Set.mem (Exp.free_variables cata_arg) name
                then
                  failwith
                    (sprintf
                       "non-structural recursion for constructor %s"
                       ctor_name)
                else cata_arg) )
      in
      Some (Exp.build_abs top_params (EApp (rs, scrutinee)))
  | _, e -> None

let rewrite : datatype_env -> env -> string -> exp option =
 fun sigma env name ->
  cata_of_definition sigma env name
  |> Option.map ~f:(nonrecursive_matches_to_catas sigma)