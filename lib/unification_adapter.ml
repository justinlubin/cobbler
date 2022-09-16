open Core
open Lang
open Unification

let rec to_unification_typ : Lang.typ -> Unification.typ =
 fun tau ->
  match tau with
  | TPlaceholder x -> Elementary x
  | TArr (domain, codomain) ->
      Arrow (to_unification_typ domain, to_unification_typ codomain)

let rec to_unification_term'
    :  Lang.typ_env -> Lang.datatype_env -> Lang.typ_env -> Lang.exp
    -> Unification.term
  =
 fun stdlib sigma gamma e ->
  match e with
  | EVar id ->
      (match String.Map.find stdlib id with
      | Some typ -> Atom (Constant (id, to_unification_typ typ))
      | None ->
          Atom
            (Variable (id, to_unification_typ (String.Map.find_exn gamma id))))
  | EApp (e1, e2) ->
      Application
        ( to_unification_term' stdlib sigma gamma e1
        , to_unification_term' stdlib sigma gamma e2 )
  | EAbs (x, tau, body) ->
      Abstraction
        ( x
        , to_unification_typ tau
        , to_unification_term'
            stdlib
            sigma
            (String.Map.update gamma x ~f:(fun _ -> tau))
            body )
  | EMatch (scrutinee, branches) ->
      let head = Util.gensym "match" in
      let datatype =
        match Type_system.infer gamma scrutinee with
        | TPlaceholder x -> x
        | TArr (_, _) -> failwith "matching on non-datatype"
      in
      let datatype_info =
        List.sort
          (String.Map.find_exn sigma datatype)
          ~compare:(fun (tag1, _) (tag2, _) -> String.compare tag1 tag2)
      in
      let codomain = Type_system.infer gamma e in
      let domain =
        TPlaceholder datatype
        :: List.map datatype_info ~f:(fun (_, typ) -> TArr (typ, codomain))
      in
      to_unification_term'
        stdlib
        sigma
        (Map.add_exn gamma ~key:head ~data:(Typ.build_arr domain codomain))
        (Exp.build_app
           (EVar head)
           (List.map2_exn
              datatype_info
              (List.sort branches ~compare:(fun (tag1, _) (tag2, _) ->
                   String.compare tag1 tag2))
              ~f:(fun (_, typ) (_, (arg_name, body)) ->
                EAbs (arg_name, typ, body))))
  | ECtor (tag, arg) ->
      let ctor_typ =
        List.find_map_exn (String.Map.to_alist sigma) ~f:(fun (dt, dt_info) ->
            Option.map
              (List.Assoc.find dt_info ~equal:String.equal tag)
              ~f:(fun domain -> TArr (domain, TPlaceholder dt)))
      in
      Application
        ( Atom (Constant (tag, to_unification_typ ctor_typ))
        , to_unification_term' stdlib sigma gamma arg )
  | EInt n -> Atom (Constant (string_of_int n, Elementary "Int"))
  | EHole (name, typ) -> Atom (Variable (name, to_unification_typ typ))

let to_unification_term : Lang.typ_env -> Lang.exp -> Unification.term =
 fun stdlib e ->
  to_unification_term' stdlib Lang.default_datatype_env String.Map.empty e

let from_unification_term : Unification.term -> Lang.exp =
 fun t -> failwith "TODO"
