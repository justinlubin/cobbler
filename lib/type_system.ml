open Core
open Lang

exception IllTyped of exp

let ctor_typ : datatype_env -> string -> string * typ =
 fun sigma tag ->
  List.find_map_exn (String.Map.to_alist sigma) ~f:(fun (dt, dt_info) ->
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
  | EApp (e1, e2) ->
      (match infer sigma gamma e1 with
      | TArr (domain, codomain) when check sigma gamma e2 domain -> codomain
      | _ -> raise (IllTyped e))
  | EAbs (x, tau, body) ->
      TArr (tau, infer sigma (String.Map.update gamma x ~f:(fun _ -> tau)) body)
  | EMatch (scrutinee, branches) ->
      (match infer sigma gamma scrutinee with
      | TDatatype dt ->
          let ctors, return_types =
            List.unzip
              (List.map branches ~f:(fun (tag, (arg_name, rhs)) ->
                   let _, domain = ctor_typ sigma tag in
                   ( (tag, domain)
                   , infer
                       sigma
                       (String.Map.update gamma arg_name ~f:(fun _ -> domain))
                       rhs )))
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
      let datatype, domain = ctor_typ sigma tag in
      if check sigma gamma arg domain
      then TDatatype datatype
      else raise (IllTyped e)
  | EUnit -> TUnit
  | EInt _ -> TInt
  | EHole (_, tau) -> tau

and check : datatype_env -> typ_env -> exp -> typ -> bool =
 fun sigma gamma e tau ->
  try [%eq: typ] (infer sigma gamma e) tau with
  | IllTyped _ -> false

let well_typed : datatype_env -> typ_env -> env -> bool =
 fun sigma gamma env ->
  String.Map.for_alli env ~f:(fun ~key:name ~data:body ->
      check
        sigma
        gamma
        body
        (Option.value_or_thunk (String.Map.find gamma name) ~default:(fun _ ->
             raise (IllTyped body))))
