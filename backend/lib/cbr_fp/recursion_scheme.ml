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
            recur
              (EApp
                 (ERScheme (RSCata RSCataNonrecursive, dt, cata_args), scrutinee))
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

let cata_of_definition_exn : datatype_env -> string -> exp -> exp =
 fun sigma name e ->
  match Exp.decompose_abs e with
  | top_params, EMatch (scrutinee, branches) ->
      let scrutinee_index, _ =
        match scrutinee with
        | EVar scrutinee_name ->
            (match
               List.findi top_params ~f:(fun _ -> String.equal scrutinee_name)
             with
            | Some i -> i
            | None -> failwith "top-level match scrutinee is not a parameter")
        | _ -> failwith "top-level match scrutinee not a variable"
      in
      let first_ctor, _ = List.hd_exn branches in
      let (dt, _), _ = Option.value_exn (Typ.ctor_typ sigma first_ctor) in
      let _, ctors = Map.find_exn sigma dt in
      let cata_args, any_args_recursive =
        List.unzip
          (List.map ctors ~f:(fun (ctor_name, domain) ->
               let branch_params, rhs =
                 List.Assoc.find_exn ~equal:String.equal branches ctor_name
               in
               let cata_arg, any_params_recursive =
                 List.fold_right
                   (List.zip_exn branch_params domain)
                   ~init:(rhs, false)
                   ~f:(fun (branch_param, branch_param_type) (acc, apr) ->
                     match branch_param_type with
                     | TDatatype (dt', _) when String.equal dt dt' ->
                         let acc_param = Util.gensym "rs_acc" in
                         let candidate =
                           EAbs
                             ( acc_param
                             , Exp.replace_subexp
                                 ~old_subexp:
                                   (Exp.build_app
                                      (EVar name)
                                      (List.mapi top_params ~f:(fun i p ->
                                           if Int.equal i scrutinee_index
                                           then EVar branch_param
                                           else EVar p)))
                                 ~new_subexp:(EVar acc_param)
                                 acc )
                         in
                         if Set.mem (Exp.free_variables candidate) branch_param
                         then
                           failwith
                             "cannot replace all references of branch \
                              parameter to accumulator"
                         else (candidate, true)
                     | _ -> (EAbs (branch_param, acc), apr))
               in
               if Set.mem (Exp.free_variables cata_arg) name
               then
                 failwith
                   (sprintf
                      "non-structural recursion for constructor %s"
                      ctor_name)
               else (cata_arg, any_params_recursive)))
      in
      let ct =
        if List.exists ~f:(fun x -> x) any_args_recursive
        then RSCataRecursive
        else RSCataNonrecursive
      in
      Exp.build_abs
        top_params
        (EApp (ERScheme (RSCata ct, dt, cata_args), scrutinee))
  | _, e -> failwith "top-level expression is not a match"

let cata_of_definition : datatype_env -> string -> exp -> exp option =
 fun sigma name e ->
  try Some (cata_of_definition_exn sigma name e) with
  | _ -> None

let rewrite : datatype_env -> string -> exp -> exp =
 fun sigma name e ->
  let e' = nonrecursive_matches_to_catas sigma e in
  e'
  |> cata_of_definition sigma name
  |> Option.value_or_thunk ~default:(fun () -> e')
