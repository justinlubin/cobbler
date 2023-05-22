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
  | ERScheme (RListFoldr (b, f)) ->
      ERScheme (RListFoldr (pull_out_cases b, pull_out_cases f))

let rec fuse' : exp -> exp =
 fun e ->
  match e with
  | EVar x -> EVar x
  (* List foldr fusion *)
  | EApp (head, EApp (ERScheme (RListFoldr (b1, f1)), arg)) ->
      let u y = Exp.normalize (EApp (head, y)) in
      let f y z = Exp.normalize (EApp (EApp (f1, y), z)) in
      (match compute_list_foldr_h ~u ~f with
      | Some h -> fuse' (EApp (ERScheme (RListFoldr (u b1, h)), arg))
      | None ->
          EApp
            ( fuse' head
            , EApp (ERScheme (RListFoldr (fuse' b1, fuse' f1)), fuse' arg) ))
  (* Match fusion (e.g. options, booleans), a.k.a. "if lifting" *)
  | EApp (head, EMatch (scrutinee, branches)) ->
      fuse'
        (EMatch
           ( scrutinee
           , Exp.map_branches branches ~f:(fun rhs -> EApp (head, rhs)) ))
  | EApp (head, arg) -> EApp (fuse' head, fuse' arg)
  | EAbs (param, body) -> EAbs (param, fuse' body)
  (* TODO: put in pull_out_cases and make it work with folds too *)
  | EMatch (scrutinee, branches) ->
      EMatch (fuse' scrutinee, Exp.map_branches branches ~f:fuse')
  | ECtor (ctor_name, args) -> ECtor (ctor_name, List.map ~f:fuse' args)
  | EBase b -> EBase b
  | EHole (name, typ) -> EHole (name, typ)
  | ERScheme (RListFoldr (b, f)) -> ERScheme (RListFoldr (fuse' b, fuse' f))

and fuse_normalize : exp -> exp =
 fun e ->
  let e' = fuse' (Exp.normalize e) in
  if [%eq: exp] e e' then e' else fuse_normalize e'

and compute_list_foldr_h : u:(exp -> exp) -> f:(exp -> exp -> exp) -> exp option
  =
 fun ~u ~f ->
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
         (fuse_normalize (u (f (EVar x) (EVar acc)))))
  in
  if String.Set.mem (Exp.free_variables h_rhs) acc
  then None
  else Some (EAbs (p_hd, EAbs (p_tl, h_rhs)))

let fuse : exp -> exp = fun e -> fuse' (Exp.freshen e)
