open Core
open Lang

let rec pull_out_cases : exp -> exp = function
  | EVar x -> EVar x
  | EApp (head, arg) -> EApp (pull_out_cases head, pull_out_cases arg)
  | EAbs (param, tau, body) -> EAbs (param, tau, pull_out_cases body)
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
  | ECtor (ctor_name, arg) -> ECtor (ctor_name, pull_out_cases arg)
  | EPair (e1, e2) -> EPair (pull_out_cases e1, pull_out_cases e2)
  | EFst arg -> EFst (pull_out_cases arg)
  | ESnd arg -> ESnd (pull_out_cases arg)
  | EUnit -> EUnit
  | EInt n -> EInt n
  | EHole (name, typ) -> EHole (name, typ)
  | ERScheme (RListFoldr (b, f), arg) ->
      ERScheme
        (RListFoldr (pull_out_cases b, pull_out_cases f), pull_out_cases arg)

let rec fuse : datatype_env -> typ_env -> exp -> exp =
 fun sigma gamma e ->
  match e with
  | EVar x -> EVar x
  | EApp (head, ERScheme (RListFoldr (b, f), arg)) ->
      (match Typ.decompose_arr (Type_system.infer sigma gamma f) with
      | [ elem_type ], _ ->
          let return_type = Type_system.infer sigma gamma e in
          let u y = EApp (head, y) in
          let h =
            compute_list_foldr_h
              sigma
              gamma
              ~elem_type
              ~return_type
              ~u
              ~f:(fun y -> EApp (f, y))
          in
          ERScheme (RListFoldr (u b, h), arg)
      | _ -> failwith "impossible list foldr type")
  | EApp (head, arg) -> EApp (fuse sigma gamma head, fuse sigma gamma arg)
  | EAbs (param, tau, body) -> EAbs (param, tau, fuse sigma gamma body)
  | EMatch (outer_scrutinee, outer_branches) -> failwith "TODO"
  | ECtor (ctor_name, arg) -> ECtor (ctor_name, fuse sigma gamma arg)
  | EPair (e1, e2) -> EPair (fuse sigma gamma e1, fuse sigma gamma e2)
  | EFst arg -> EFst (fuse sigma gamma arg)
  | ESnd arg -> ESnd (fuse sigma gamma arg)
  | EUnit -> EUnit
  | EInt n -> EInt n
  | EHole (name, typ) -> EHole (name, typ)
  | ERScheme (RListFoldr (b2, f2), ERScheme (RListFoldr (b1, f1), arg)) ->
      (match Typ.decompose_arr (Type_system.infer sigma gamma f1) with
      | [ elem_type ], _ ->
          let return_type = Type_system.infer sigma gamma e in
          let u y = ERScheme (RListFoldr (b2, f2), y) in
          let h =
            compute_list_foldr_h
              sigma
              gamma
              ~elem_type
              ~return_type
              ~u
              ~f:(fun y -> EApp (f1, y))
          in
          ERScheme (RListFoldr (u b1, h), arg)
      | _ -> failwith "impossible list foldr type")
  | ERScheme (RListFoldr (b, f), arg) ->
      ERScheme
        ( RListFoldr (fuse sigma gamma b, fuse sigma gamma f)
        , fuse sigma gamma arg )

and fuse_normalize : datatype_env -> typ_env -> exp -> exp =
 fun sigma gamma e ->
  let e' = fuse sigma gamma (Exp.normalize e) in
  if [%eq: exp] e e' then e' else fuse_normalize sigma gamma e'

and compute_list_foldr_h
    :  datatype_env -> typ_env -> elem_type:typ -> return_type:typ
    -> u:(exp -> exp) -> f:(exp -> exp) -> exp
  =
 fun sigma gamma ~elem_type ~return_type ~u ~f ->
  let x = Util.gensym "fuse_list_foldr_x" in
  let acc = Util.gensym "fuse_list_foldr_acc" in
  let p = Util.gensym "fuse_list_foldr_p" in
  let h_rhs =
    Exp.replace_subexp
      ~old_subexp:(EVar x)
      ~new_subexp:(EFst (EVar p))
      (Exp.replace_subexp
         ~old_subexp:(u (EVar acc))
         ~new_subexp:(ESnd (EVar p))
         (fuse_normalize sigma gamma (u (f (EPair (EVar x, EVar acc))))))
  in
  if String.Set.mem (Exp.free_variables h_rhs) acc
  then failwith "could not find closed form for h"
  else EAbs (p, TProd (elem_type, return_type), h_rhs)
