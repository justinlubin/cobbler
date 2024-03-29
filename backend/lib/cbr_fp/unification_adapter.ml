open Core
open Lang
open Unification

(* Types *)

let rec to_unification_typ : Lang.typ -> Unification.typ = function
  | TBase b -> Elementary (TBase b)
  | TVar x -> Elementary (TVar x)
  | TDatatype (x, taus) -> Elementary (TDatatype (x, taus))
  | TArr (domain, codomain) ->
      Arrow (to_unification_typ domain, to_unification_typ codomain)

let rec from_unification_typ : Unification.typ -> Lang.typ = function
  | Elementary tau -> tau
  | Arrow (domain, codomain) ->
      TArr (from_unification_typ domain, from_unification_typ codomain)

(* Terms *)

let sort_tags : 'a. (string * 'a) list -> (string * 'a) list =
 fun bs ->
  List.sort ~compare:(fun (tag1, _) (tag2, _) -> String.compare tag1 tag2) bs

let rec embed
    :  Lang.datatype_env -> String.Set.t -> Lang.typ_env -> string -> string
    -> Lang.typ -> Lang.exp list -> Unification.term
  =
 fun sigma stdlib gamma prefix metadata result_type arguments ->
  let head = Util.embed_name prefix metadata in
  to_unification_term'
    sigma
    (Set.add stdlib head)
    (Map.add_exn
       gamma
       ~key:head
       ~data:
         ( []
         , Typ.build_arr
             (List.map ~f:(Type_system.infer sigma gamma) arguments)
             result_type ))
    (Exp.build_app (EVar head) arguments)

and to_unification_term'
    :  Lang.datatype_env -> String.Set.t -> Lang.typ_env -> Lang.exp
    -> Unification.term
  =
 fun sigma stdlib gamma e ->
  let result_type = Type_system.infer sigma gamma e in
  let embed' prefix metadata arguments =
    embed sigma stdlib gamma prefix metadata result_type arguments
  in
  match e with
  | EVar x ->
      if Set.mem stdlib x
      then Atom (Constant (x, to_unification_typ result_type))
      else Atom (Variable (x, to_unification_typ result_type))
  | EApp (e1, e2) ->
      Application
        ( to_unification_term' sigma stdlib gamma e1
        , to_unification_term' sigma stdlib gamma e2 )
  | EAbs (x, body) ->
      (match result_type with
      | TArr (dom, _) ->
          Abstraction
            ( x
            , to_unification_typ dom
            , to_unification_term'
                sigma
                (Set.remove stdlib x)
                (Map.update gamma x ~f:(fun _ -> ([], dom)))
                body )
      | _ -> failwith "improper abstraction type")
  | EMatch (scrutinee, branches) ->
      let arguments =
        List.map (sort_tags branches) ~f:(fun (tag, (arg_names, rhs)) ->
            Exp.build_abs arg_names rhs)
      in
      embed' "match" "" (scrutinee :: arguments)
  | ECtor (tag, args) -> embed' "ctor" tag args
  | EBase (BEInt n) -> embed' "base_int" (Int.to_string n) []
  | EBase (BEFloat f) -> embed' "base_float" (Float.to_string f) []
  | EBase (BEString s) -> embed' "base_string" s []
  | EHole (name, typ) ->
      Atom (Variable (sprintf "__ua_hole^%s" name, to_unification_typ typ))
  | ERScheme (RSCata ct, dt, args) ->
      let flag =
        match ct with
        | RSCataNonrecursive -> 0
        | RSCataRecursive -> 1
      in
      embed' "cata" dt (EBase (BEInt flag) :: args)

let to_unification_term
    : Lang.datatype_env -> Lang.typ_env -> Lang.exp -> Unification.term
  =
 fun sigma stdlib e -> to_unification_term' sigma (Map.key_set stdlib) stdlib e

let rec from_unification_term
    : Lang.datatype_env -> Unification.term -> Lang.exp
  =
 fun sigma t ->
  let heading, head, arguments = Unification.abbreviate t in
  let build_arguments x =
    Exp.build_app x (List.map ~f:(from_unification_term sigma) arguments)
  in
  let body =
    match head with
    | Variable (x, tau) ->
        (match String.chop_prefix ~prefix:"__ua_hole^" x with
        | Some name -> build_arguments (EHole (name, from_unification_typ tau))
        | None -> build_arguments (EVar x))
    | Constant (x, _) ->
        (match Util.unembed_name x with
        | Some ("match", "") ->
            let scrutinee_term = List.hd_exn arguments in
            let branch_terms = List.tl_exn arguments in
            let datatype =
              match from_unification_typ (Unification.typ scrutinee_term) with
              | TDatatype (x, _) -> x
              | _ -> failwith "ill-typed scrutinee"
            in
            let tags =
              datatype
              |> Map.find_exn sigma
              |> snd
              |> sort_tags
              |> List.map ~f:(fun (tag, _) -> tag)
            in
            EMatch
              ( from_unification_term sigma scrutinee_term
              , List.map2_exn tags branch_terms ~f:(fun tag t ->
                    let xs, body = Unification.strip_abstractions t in
                    (tag, (List.map ~f:fst xs, from_unification_term sigma body)))
              )
        | Some ("ctor", tag) ->
            ECtor (tag, List.map ~f:(from_unification_term sigma) arguments)
        | Some ("base_int", n) -> EBase (BEInt (Int.of_string n))
        | Some ("base_float", f) -> EBase (BEFloat (Float.of_string f))
        | Some ("base_string", s) -> EBase (BEString s)
        | Some ("cata", dt) ->
            (* Only supports applied catas *)
            let first_arg = List.hd_exn arguments in
            let middle_args = arguments |> List.tl_exn |> List.drop_last_exn in
            let last_arg = List.last_exn arguments in
            EApp
              ( ERScheme
                  ( RSCata
                      (match from_unification_term sigma first_arg with
                      | EBase (BEInt 0) -> RSCataNonrecursive
                      | _ -> RSCataRecursive)
                  , dt
                  , List.map ~f:(from_unification_term sigma) middle_args )
              , from_unification_term sigma last_arg )
        | _ -> build_arguments (EVar x))
  in
  Exp.build_abs (List.map ~f:fst heading) body

let simplify_solution
    :  datatype_env -> (string * Unification.term) list
    -> (string * Lang.exp) list
  =
 fun sigma subs ->
  List.filter_map subs ~f:(fun (lhs, rhs) ->
      match String.chop_prefix ~prefix:"__ua_hole^" lhs with
      | Some name ->
          Some
            ( name
            , from_unification_term
                sigma
                (Unification.normalize
                   (Unification.substitute_recursively subs rhs)) )
      | None -> None)
