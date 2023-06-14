open Core
open Lang

let rec fuse' : datatype_env -> exp -> exp =
 fun sigma e ->
  let rec recur = function
    | EVar x -> EVar x
    (* Catamorphism fusion *)
    | EApp (u, EApp (ERScheme (RSCata, dt, fs), arg)) ->
        (match
           Option.all
             (List.map2_exn
                ~f:(fun f (_, domain) ->
                  compute_new_cata_arg sigma ~u ~f dt domain)
                fs
                (snd (Map.find_exn sigma dt)))
         with
        | Some new_fs -> recur (EApp (ERScheme (RSCata, dt, new_fs), arg))
        | None ->
            EApp
              ( recur u
              , EApp (ERScheme (RSCata, dt, List.map ~f:recur fs), recur arg) ))
    (* Match fusion (e.g. options, booleans), a.k.a. "if lifting" *)
    | EApp (head, EMatch (scrutinee, branches)) ->
        recur
          (EMatch
             ( scrutinee
             , Exp.map_branches branches ~f:(fun rhs -> EApp (head, rhs)) ))
    | EApp (head, arg) -> EApp (recur head, recur arg)
    | EAbs (param, body) -> EAbs (param, recur body)
    | EMatch (scrutinee, branches) ->
        EMatch (recur scrutinee, Exp.map_branches branches ~f:recur)
    | ECtor (ctor_name, args) -> ECtor (ctor_name, List.map ~f:recur args)
    | EBase b -> EBase b
    | EHole (name, typ) -> EHole (name, typ)
    | ERScheme (rs, dt, args) -> ERScheme (rs, dt, List.map ~f:recur args)
  in
  recur e

and fuse_normalize : datatype_env -> exp -> exp =
 fun sigma e ->
  let e' = fuse' sigma (Exp.normalize sigma e) in
  if [%eq: exp] e e' then e' else fuse_normalize sigma e'

and compute_new_cata_arg
    : datatype_env -> u:exp -> f:exp -> string -> typ list -> exp option
  =
 fun sigma ~u ~f dt domain ->
  let xzs =
    List.map
      ~f:(fun tau -> (Util.gensym "cata_x", Util.gensym "cata_z", tau))
      domain
  in
  let application =
    fuse_normalize
      sigma
      (EApp (u, Exp.build_app f (List.map ~f:(fun (x, _, _) -> EVar x) xzs)))
  in
  let recursive_xs, rhs =
    List.fold_right
      ~f:(fun (x, z, tau) (acc_xs, acc_app) ->
        match tau with
        | TDatatype (dt', _) when String.equal dt dt' ->
            ( Set.add acc_xs x
            , Exp.replace_subexp
                ~old_subexp:(Exp.normalize sigma (EApp (u, EVar x)))
                ~new_subexp:(EVar z)
                acc_app )
        | _ ->
            ( acc_xs
            , Exp.replace_subexp
                ~old_subexp:(EVar x)
                ~new_subexp:(EVar z)
                acc_app ))
      ~init:(String.Set.empty, application)
      xzs
  in
  if Set.is_empty (Set.inter (Exp.free_variables rhs) recursive_xs)
  then Some (Exp.build_abs (List.map ~f:(fun (_, z, _) -> z) xzs) rhs)
  else None

let fuse : datatype_env -> exp -> exp =
 fun sigma e -> fuse' sigma (Exp.freshen e)
