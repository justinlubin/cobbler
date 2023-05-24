open Core
open Lang

let extract_cata_exn : datatype_env -> typ_env -> env -> string -> exp =
 fun sigma gamma env name ->
  match Exp.decompose_abs (String.Map.find_exn env name) with
  | top_params, EMatch (scrutinee, branches) ->
      let first_ctor, _ = List.hd_exn branches in
      let (dt, _), _ = Option.value_exn (Typ.ctor_typ sigma first_ctor) in
      let _, ctors = Map.find_exn sigma dt in
      Exp.build_abs
        top_params
        (EApp
           ( ERScheme
               ( RSCata
               , dt
               , List.map ctors ~f:(fun (ctor_name, domain) ->
                     let branch_params, rhs =
                       List.Assoc.find_exn
                         ~equal:String.equal
                         branches
                         ctor_name
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
           , scrutinee ))
  | _, e ->
      failwith
        (sprintf
           "non-match under top-level abstractions %s"
           (Exp.show_multi 0 e))

let extract_cata : datatype_env -> typ_env -> env -> string -> exp option =
 fun sigma gamma env name ->
  try Some (extract_cata_exn sigma gamma env name) with
  | Failure s -> None
