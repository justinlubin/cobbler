open Core
open Lang

let rec pull_out_cases : exp -> exp = function
  | EVar x -> EVar x
  | EApp (head, arg) -> EApp (pull_out_cases head, pull_out_cases arg)
  | EAbs (param, body) -> EAbs (param, pull_out_cases body)
  | EMatch (outer_scrutinee, outer_branches) ->
      let outer_branches' = Exp.map_branches ~f:pull_out_cases outer_branches in
      (match pull_out_cases outer_scrutinee with
      | EMatch (inner_scrutinee, inner_branches) ->
          EMatch
            ( inner_scrutinee
            , Exp.map_branches
                ~f:(fun rhs -> EMatch (rhs, outer_branches'))
                inner_branches )
      | outer_scrutinee' -> EMatch (outer_scrutinee', outer_branches'))
  | ECtor (ctor_name, args) -> ECtor (ctor_name, List.map ~f:pull_out_cases args)
  | EBase b -> EBase b
  | EHole (name, typ) -> EHole (name, typ)
  | ERScheme (rs, dt, args) -> ERScheme (rs, dt, List.map ~f:pull_out_cases args)

let rec fuse' : datatype_env -> exp -> exp =
 fun sigma e ->
  let rec recur = function
    | EVar x -> EVar x
    (* Catamorphism fusion *)
    | EApp (u, EApp (ERScheme (RSCata, dt, es), arg)) -> failwith "TODO"
    (*let u y = Exp.normalize (EApp (head, y)) in
        let f y z = Exp.normalize (EApp (EApp (f1, y), z)) in
        (match compute_list_foldr_h ~u ~f with
        | Some h -> recur (EApp (ERScheme (RListFoldr (u b1, h)), arg))
        | None ->
            EApp
              ( recur head
              , EApp (ERScheme (RListFoldr (fuse' b1, fuse' f1)), recur arg) ))*)
    (* Match fusion (e.g. options, booleans), a.k.a. "if lifting" *)
    | EApp (head, EMatch (scrutinee, branches)) ->
        recur
          (EMatch
             ( scrutinee
             , Exp.map_branches branches ~f:(fun rhs -> EApp (head, rhs)) ))
    | EApp (head, arg) -> EApp (recur head, recur arg)
    | EAbs (param, body) -> EAbs (param, recur body)
    (* TODO: put in pull_out_cases and make it work with folds too *)
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

and compute_list_foldr_h
    : datatype_env -> u:(exp -> exp) -> f:(exp -> exp -> exp) -> exp option
  =
 fun sigma ~u ~f ->
  let x = Util.gensym "fuse_list_foldr_x" in
  let acc = Util.gensym "fuse_list_foldr_acc" in
  let p_hd = Util.gensym "fuse_list_foldr_p_hd" in
  let p_tl = Util.gensym "fuse_list_foldr_p_tl" in
  let h_rhs =
    Exp.replace_subexp
      ~old_subexp:(EVar x)
      ~new_subexp:(EVar p_hd)
      (Exp.replace_subexp
         ~old_subexp:(u (EVar acc))
         ~new_subexp:(EVar p_tl)
         (fuse_normalize sigma (u (f (EVar x) (EVar acc)))))
  in
  if String.Set.mem (Exp.free_variables h_rhs) acc
  then None
  else Some (EAbs (p_hd, EAbs (p_tl, h_rhs)))

let fuse : datatype_env -> exp -> exp =
 fun sigma e -> fuse' sigma (Exp.freshen e)
