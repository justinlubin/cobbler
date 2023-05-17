open Core
open Lang

(* This module implements type inference via HM-style constraint generation
   and checking. See, for example, TAPL Chapter 22. *)

(* Type helpers *)

let rec free_vars : typ -> String.Set.t =
 fun tau ->
  match tau with
  | TUnit -> String.Set.empty
  | TInt -> String.Set.empty
  | TVar x -> String.Set.singleton x
  | TDatatype (_, args) ->
      args |> List.map ~f:free_vars |> String.Set.union_list
  | TProd (left, right) -> String.Set.union (free_vars left) (free_vars right)
  | TArr (dom, cod) -> String.Set.union (free_vars dom) (free_vars cod)

let ctor_typ : datatype_env -> string -> (string * string list * typ) option =
 fun sigma tag ->
  List.find_map
    (String.Map.to_alist sigma)
    ~f:(fun (dt, (dt_params, dt_info)) ->
      Option.map
        (List.Assoc.find dt_info ~equal:String.equal tag)
        ~f:(fun domain -> (dt, dt_params, domain)))

let fresh_type_var : unit -> typ = fun () -> TVar (Util.gensym "__typevar")

(* Type substitions (no need to worry about variable capture since we don't
   have fully-fledged universal polymorphism) *)

type sub = typ String.Map.t

let rec apply_sub : sub -> typ -> typ =
 fun sigma tau ->
  match tau with
  | TUnit -> TUnit
  | TInt -> TInt
  | TVar x ->
      (match Map.find sigma x with
      | None -> TVar x
      | Some t -> t)
  | TDatatype (dt, args) -> TDatatype (dt, List.map ~f:(apply_sub sigma) args)
  | TProd (left, right) -> TProd (apply_sub sigma left, apply_sub sigma right)
  | TArr (dom, cod) -> TArr (apply_sub sigma dom, apply_sub sigma cod)

let apply_sub_env : sub -> typ_env -> typ_env =
 fun sigma gamma -> Map.map ~f:(apply_sub sigma) gamma

let compose_subs : sub -> sub -> sub =
 fun sigma1 sigma2 ->
  Map.merge sigma1 sigma2 ~f:(fun ~key el ->
      match el with
      | `Left t1 -> Some t1
      | `Right t2 | `Both (_, t2) -> Some (apply_sub sigma1 t2))

(* Type constraints *)

type constraint_set = (typ * typ) list

let apply_sub_constraints : sub -> constraint_set -> constraint_set =
 fun sigma cs ->
  List.map ~f:(fun (s, t) -> (apply_sub sigma s, apply_sub sigma t)) cs

(* Constraint typing *)

exception IllTyped of exp [@@deriving sexp]

let rec constraint_type : datatype_env -> typ_env -> exp -> typ * constraint_set
  =
 fun sigma gamma e ->
  match e with
  | EVar x ->
      (match Map.find gamma x with
      | None -> raise (IllTyped e)
      | Some t -> (t, []))
  | EApp (e1, e2) ->
      let t1, c1 = constraint_type sigma gamma e1 in
      let t2, c2 = constraint_type sigma gamma e2 in
      let x = fresh_type_var () in
      (x, ((t1, TArr (t2, x)) :: c1) @ c2)
  | EAbs (param, t1, body) ->
      let t2, c =
        constraint_type sigma (Map.update gamma param ~f:(fun _ -> t1)) body
      in
      (TArr (t1, t2), c)
  | EMatch (scrutinee, branches) ->
      (match branches with
      | [] -> raise (IllTyped e)
      | (first_ctor, _) :: _ ->
          (match ctor_typ sigma first_ctor with
          | Some (dt, dt_params, _) ->
              let dt_sub_list =
                List.map ~f:(fun p -> (p, fresh_type_var ())) dt_params
              in
              let dt_sub = String.Map.of_alist_exn dt_sub_list in
              let ctors, rhs_types, rhs_constraints =
                List.unzip3
                  (List.map branches ~f:(fun (tag, (arg_name, rhs)) ->
                       match ctor_typ sigma tag with
                       | Some (_, _, domain) ->
                           let t_rhs, c_rhs =
                             constraint_type
                               sigma
                               (String.Map.update gamma arg_name ~f:(fun _ ->
                                    apply_sub dt_sub domain))
                               rhs
                           in
                           (tag, t_rhs, c_rhs)
                       | None -> raise (IllTyped e)))
              in
              if List.equal
                   [%eq: id]
                   (List.sort ctors ~compare:[%compare: id])
                   (List.sort
                      (List.map ~f:fst (snd (String.Map.find_exn sigma dt)))
                      ~compare:[%compare: id])
              then (
                let t_scrutinee, c_scrutinee =
                  constraint_type sigma gamma scrutinee
                in
                let return_type = fresh_type_var () in
                ( return_type
                , ((t_scrutinee, TDatatype (dt, List.map ~f:snd dt_sub_list))
                  :: c_scrutinee)
                  @ List.map ~f:(fun t -> (return_type, t)) rhs_types
                  @ List.concat rhs_constraints ))
              else raise (IllTyped e)
          | None -> raise (IllTyped e)))
  | ECtor (tag, body) ->
      let t_body, c_body = constraint_type sigma gamma body in
      (match ctor_typ sigma tag with
      | Some (dt, params, domain) ->
          let sub_list = List.map ~f:(fun p -> (p, fresh_type_var ())) params in
          let sub = String.Map.of_alist_exn sub_list in
          ( TDatatype (dt, List.map ~f:snd sub_list)
          , (apply_sub sub domain, t_body) :: c_body )
      | None -> raise (IllTyped e))
  | EPair (e1, e2) ->
      let t1, c1 = constraint_type sigma gamma e1 in
      let t2, c2 = constraint_type sigma gamma e2 in
      (TProd (t1, t2), c1 @ c2)
  | EFst arg ->
      let t_arg, c_arg = constraint_type sigma gamma arg in
      let x = fresh_type_var () in
      let y = fresh_type_var () in
      (x, (TProd (x, y), t_arg) :: c_arg)
  | ESnd arg ->
      let t_arg, c_arg = constraint_type sigma gamma arg in
      let x = fresh_type_var () in
      let y = fresh_type_var () in
      (y, (TProd (x, y), t_arg) :: c_arg)
  | EUnit -> (TUnit, [])
  | EInt _ -> (TInt, [])
  | EHole (_, t) -> (t, [])
  | ERScheme (RListFoldr (b, f)) ->
      (* TODO: assumes arguments are correct *)
      let t_b, c_b = constraint_type sigma gamma b in
      let _t_f, c_f = constraint_type sigma gamma f in
      let x = fresh_type_var () in
      (TArr (x, t_b), c_b @ c_f)

(* Constraint unification *)

exception CannotUnify of (typ * typ) list [@@deriving sexp]

let rec unify : constraint_set -> sub =
 fun cs ->
  match cs with
  | [] -> String.Map.empty
  | (s, t) :: tail ->
      let fvs = free_vars s in
      let fvt = free_vars t in
      (match (s, t) with
      | TUnit, TUnit | TInt, TInt -> unify tail
      | TVar x, _ when not (String.Set.mem fvt x) ->
          let sub = String.Map.singleton x t in
          compose_subs (unify (apply_sub_constraints sub tail)) sub
      | _, TVar x when not (String.Set.mem fvs x) ->
          let sub = String.Map.singleton x s in
          compose_subs (unify (apply_sub_constraints sub tail)) sub
      | TDatatype (dt1, args1), TDatatype (dt2, args2) when String.equal dt1 dt2
        ->
          (match List.map2 ~f:(fun a1 a2 -> (a1, a2)) args1 args2 with
          | List.Or_unequal_lengths.Unequal_lengths -> raise (CannotUnify cs)
          | List.Or_unequal_lengths.Ok arg_constraints ->
              unify (arg_constraints @ tail))
      | TProd (left1, right1), TProd (left2, right2) ->
          unify ((left1, left2) :: (right1, right2) :: tail)
      | TArr (dom1, cod1), TArr (dom2, cod2) ->
          unify ((dom1, dom2) :: (cod1, cod2) :: tail)
      | TUnit, _
      | TInt, _
      | TVar _, _
      | TDatatype (_, _), _
      | TArr (_, _), _
      | TProd (_, _), _ -> raise (CannotUnify cs))

(* Type system interface *)

let infer : datatype_env -> typ_env -> exp -> typ =
 fun sigma gamma e ->
  let s, c = constraint_type sigma gamma e in
  let sub = unify c in
  apply_sub sub s

let check : datatype_env -> typ_env -> exp -> typ -> unit =
 fun sigma gamma e tau ->
  let _ = unify [ (infer sigma gamma e, tau) ] in
  ()

let well_typed : datatype_env * typ_env * env -> unit =
 fun (sigma, gamma, env) ->
  String.Map.iteri env ~f:(fun ~key:name ~data:body ->
      check
        sigma
        gamma
        body
        (Option.value_or_thunk (String.Map.find gamma name) ~default:(fun _ ->
             raise (IllTyped body))))
