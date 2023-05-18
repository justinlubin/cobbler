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
  | ECtor (ctor_name, args) -> ECtor (ctor_name, List.map ~f:pull_out_cases args)
  | EPair (e1, e2) -> EPair (pull_out_cases e1, pull_out_cases e2)
  | EFst arg -> EFst (pull_out_cases arg)
  | ESnd arg -> ESnd (pull_out_cases arg)
  | EUnit -> EUnit
  | EInt n -> EInt n
  | EHole (name, typ) -> EHole (name, typ)
  | ERScheme (RListFoldr (b, f)) ->
      ERScheme (RListFoldr (pull_out_cases b, pull_out_cases f))

let rec fuse' : datatype_env -> typ_env -> exp -> exp =
 fun sigma gamma e ->
  match e with
  | EVar x -> EVar x
  (* List foldr fusion *)
  | EApp (head, EApp (ERScheme (RListFoldr (b1, f1)), arg)) ->
      (match Typ.decompose_arr (Type_system.infer sigma gamma f1) with
      | [ elem_type; _ ], _ ->
          let return_type = Type_system.infer sigma gamma e in
          let u y = Exp.normalize (EApp (head, y)) in
          let f y z = Exp.normalize (EApp (EApp (f1, y), z)) in
          (match
             compute_list_foldr_h sigma gamma ~elem_type ~return_type ~u ~f
           with
          | Some h ->
              fuse' sigma gamma (EApp (ERScheme (RListFoldr (u b1, h)), arg))
          | None ->
              EApp
                ( fuse' sigma gamma head
                , EApp
                    ( ERScheme
                        (RListFoldr (fuse' sigma gamma b1, fuse' sigma gamma f1))
                    , fuse' sigma gamma arg ) ))
      | _ -> failwith "impossible list foldr type")
  (* Match fusion (e.g. options, booleans), a.k.a. "if lifting" *)
  | EApp (head, EMatch (scrutinee, branches)) ->
      fuse'
        sigma
        gamma
        (EMatch
           ( scrutinee
           , Exp.map_branches branches ~f:(fun rhs -> EApp (head, rhs)) ))
  | EApp (head, arg) -> EApp (fuse' sigma gamma head, fuse' sigma gamma arg)
  | EAbs (param, tau, body) ->
      EAbs
        ( param
        , tau
        , fuse' sigma (String.Map.update gamma param ~f:(fun _ -> tau)) body )
  (* TODO: put in pull_out_cases and make it work with folds too *)
  | EMatch (scrutinee, branches) ->
      EMatch
        ( fuse' sigma gamma scrutinee
        , Exp.map_branches branches ~f:(fuse' sigma gamma) )
  | ECtor (ctor_name, args) ->
      ECtor (ctor_name, List.map ~f:(fuse' sigma gamma) args)
  | EPair (e1, e2) -> EPair (fuse' sigma gamma e1, fuse' sigma gamma e2)
  | EFst arg -> EFst (fuse' sigma gamma arg)
  | ESnd arg -> ESnd (fuse' sigma gamma arg)
  | EUnit -> EUnit
  | EInt n -> EInt n
  | EHole (name, typ) -> EHole (name, typ)
  | ERScheme (RListFoldr (b, f)) ->
      ERScheme (RListFoldr (fuse' sigma gamma b, fuse' sigma gamma f))

and fuse_normalize : datatype_env -> typ_env -> exp -> exp =
 fun sigma gamma e ->
  let e' = fuse' sigma gamma (Exp.normalize e) in
  if [%eq: exp] e e' then e' else fuse_normalize sigma gamma e'

and compute_list_foldr_h
    :  datatype_env -> typ_env -> elem_type:typ -> return_type:typ
    -> u:(exp -> exp) -> f:(exp -> exp -> exp) -> exp option
  =
 fun sigma gamma ~elem_type ~return_type ~u ~f ->
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
         (fuse_normalize
            sigma
            (String.Map.add_exn
               (String.Map.add_exn gamma ~key:x ~data:elem_type)
               ~key:acc
               ~data:return_type)
            (u (f (EVar x) (EVar acc)))))
  in
  if String.Set.mem (Exp.free_variables h_rhs) acc
  then None
  else Some (EAbs (p_hd, elem_type, EAbs (p_tl, return_type, h_rhs)))

let fuse : datatype_env -> typ_env -> exp -> exp =
 fun sigma gamma e -> fuse' sigma gamma (Exp.freshen e)
