open Core
open Lang

exception IllTyped of exp

let ctor_typ : datatype_env -> string -> (string * typ) option =
 fun sigma tag ->
  List.find_map (String.Map.to_alist sigma) ~f:(fun (dt, dt_info) ->
      Option.map
        (List.Assoc.find dt_info ~equal:String.equal tag)
        ~f:(fun domain -> (dt, domain)))

let rec infer : datatype_env -> typ_env -> exp -> typ =
 fun sigma gamma e ->
  match e with
  | EVar x ->
      (match String.Map.find gamma x with
      | None -> raise (IllTyped e)
      | Some tau -> tau)
  | EApp (ERScheme (RListFoldr (b, f)), arg) ->
      let return_type = infer sigma gamma b in
      (match infer sigma gamma arg with
      | TDatatype dt ->
          (match String.Map.find sigma dt with
          | Some variants ->
              (match
                 List.sort
                   ~compare:(fun (x1, _) (x2, _) -> String.compare x1 x2)
                   variants
               with
              | [ ("Cons", TProd (elem_type, TDatatype dt')); ("Nil", TUnit) ]
                when String.equal dt dt' ->
                  check
                    sigma
                    gamma
                    f
                    (TArr (TProd (elem_type, return_type), return_type));
                  return_type
              | _ -> raise (IllTyped e))
          | _ -> raise (IllTyped e))
      | _ -> raise (IllTyped e))
  | EApp (e1, e2) ->
      (match infer sigma gamma e1 with
      | TArr (domain, codomain) ->
          check sigma gamma e2 domain;
          codomain
      | _ -> raise (IllTyped e))
  | EAbs (x, tau, body) ->
      TArr (tau, infer sigma (String.Map.update gamma x ~f:(fun _ -> tau)) body)
  | EMatch (scrutinee, branches) ->
      (match infer sigma gamma scrutinee with
      | TDatatype dt ->
          let ctors, return_types =
            List.unzip
              (List.map branches ~f:(fun (tag, (arg_name, rhs)) ->
                   match ctor_typ sigma tag with
                   | Some (_, domain) ->
                       ( (tag, domain)
                       , infer
                           sigma
                           (String.Map.update gamma arg_name ~f:(fun _ ->
                                domain))
                           rhs )
                   | None -> raise (IllTyped e)))
          in
          if List.equal
               [%eq: id * typ]
               (List.sort ctors ~compare:[%compare: id * typ])
               (List.sort
                  (String.Map.find_exn sigma dt)
                  ~compare:[%compare: id * typ])
          then (
            match List.all_equal return_types ~equal:[%eq: typ] with
            | Some tau -> tau
            | None -> raise (IllTyped e))
          else raise (IllTyped e)
      | _ -> raise (IllTyped e))
  | ECtor (tag, arg) ->
      (match ctor_typ sigma tag with
      | Some (datatype, domain) ->
          check sigma gamma arg domain;
          TDatatype datatype
      | None -> raise (IllTyped e))
  | EPair (e1, e2) -> TProd (infer sigma gamma e1, infer sigma gamma e2)
  | EFst arg ->
      (match infer sigma gamma arg with
      | TProd (tau1, _) -> tau1
      | _ -> raise (IllTyped e))
  | ESnd arg ->
      (match infer sigma gamma arg with
      | TProd (_, tau2) -> tau2
      | _ -> raise (IllTyped e))
  | EUnit -> TUnit
  | EInt _ -> TInt
  | EHole (_, tau) -> tau
  | ERScheme _ -> raise (IllTyped e)

and check : datatype_env -> typ_env -> exp -> typ -> unit =
 fun sigma gamma e tau ->
  if [%eq: typ] (infer sigma gamma e) tau then () else raise (IllTyped e)

let well_typed : datatype_env * typ_env * env -> unit =
 fun (sigma, gamma, env) ->
  String.Map.iteri env ~f:(fun ~key:name ~data:body ->
      check
        sigma
        gamma
        body
        (Option.value_or_thunk (String.Map.find gamma name) ~default:(fun _ ->
             raise (IllTyped body))))
