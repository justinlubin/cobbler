open Core
open Lang
open Unification

(* Types *)

let rec to_unification_typ : Lang.typ -> Unification.typ = function
  | TUnit -> Elementary "Unit"
  | TInt -> Elementary "Int"
  | TDatatype x -> Elementary x
  | TArr (domain, codomain) ->
      Arrow (to_unification_typ domain, to_unification_typ codomain)

let rec from_unification_typ : Unification.typ -> Lang.typ = function
  | Elementary "Unit" -> TUnit
  | Elementary "Int" -> TInt
  | Elementary x -> TDatatype x
  | Arrow (domain, codomain) ->
      TArr (from_unification_typ domain, from_unification_typ codomain)

(* Terms *)

let rec embed
    :  Lang.datatype_env -> String.Set.t -> Lang.typ_env -> string -> Lang.typ
    -> Lang.exp list -> Unification.term
  =
 fun sigma stdlib gamma prefix result_type arguments ->
  let head = Util.gensym prefix in
  to_unification_term'
    sigma
    (String.Set.add stdlib head)
    (String.Map.add_exn
       gamma
       ~key:head
       ~data:
         (Typ.build_arr
            (List.map ~f:(Type_system.infer sigma gamma) arguments)
            result_type))
    (Exp.build_app (EVar head) arguments)

and to_unification_term'
    :  Lang.datatype_env -> String.Set.t -> Lang.typ_env -> Lang.exp
    -> Unification.term
  =
 fun sigma stdlib gamma e ->
  match e with
  | EVar x ->
      let tau = to_unification_typ (String.Map.find_exn gamma x) in
      if String.Set.mem stdlib x
      then Atom (Constant (x, tau))
      else Atom (Variable (x, tau))
  | EApp (e1, e2) ->
      Application
        ( to_unification_term' sigma stdlib gamma e1
        , to_unification_term' sigma stdlib gamma e2 )
  | EAbs (x, tau, body) ->
      Abstraction
        ( x
        , to_unification_typ tau
        , to_unification_term'
            sigma
            (String.Set.remove stdlib x)
            (String.Map.update gamma x ~f:(fun _ -> tau))
            body )
  | EMatch (scrutinee, branches) ->
      let result_type = Type_system.infer sigma gamma e in
      let arguments =
        List.map branches ~f:(fun (tag, (arg_name, rhs)) ->
            EAbs
              ( arg_name
              , snd (Option.value_exn (Type_system.ctor_typ sigma tag))
              , rhs ))
      in
      embed sigma stdlib gamma "match" result_type arguments
  | ECtor (tag, arg) ->
      (* TODO: left off here! *)
      (* let datatype, arg_type = Type_system.ctor_typ in
      let ctor_typ =
        List.find_map_exn (String.Map.to_alist sigma) ~f:(fun (dt, dt_info) ->
            Option.map
              (List.Assoc.find dt_info ~equal:String.equal tag)
              ~f:(fun domain -> TArr (domain, TDatatype dt)))
      in
      Application
        ( Atom (Constant ("__ctor$" ^ tag, to_unification_typ ctor_typ))
        , to_unification_term' sigma stdlib gamma arg ) *)
      failwith "TODO"
  | EUnit -> Atom (Constant ("__unit$", to_unification_typ TUnit))
  | EInt n -> Atom (Constant (string_of_int n, to_unification_typ TInt))
  | EHole (name, typ) ->
      Atom (Variable ("__??$" ^ name, to_unification_typ typ))

let to_unification_term
    : Lang.datatype_env -> Lang.typ_env -> Lang.exp -> Unification.term
  =
 fun sigma stdlib e ->
  to_unification_term' sigma (String.Map.key_set stdlib) stdlib e

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
    | Variable (x, tau) when String.is_prefix ~prefix:"__??" x ->
        build_arguments
          (EHole
             (String.chop_prefix_exn ~prefix:"__??$" x, from_unification_typ tau))
    | Variable (x, _) -> build_arguments (EVar x)
    | Constant ("__unit$", _) -> EUnit
    | Constant (x, _) when String.is_prefix ~prefix:"__stdlib" x ->
        build_arguments (EVar (String.chop_prefix_exn ~prefix:"__stdlib$" x))
    | Constant (x, _) when String.is_prefix ~prefix:"__ctor" x ->
        ECtor
          ( String.chop_prefix_exn ~prefix:"__ctor$" x
          , from_unification_term sigma (List.hd_exn arguments) )
    | Constant (x, _) when String.is_prefix ~prefix:"__match" x ->
        let scrutinee_term = List.hd_exn arguments in
        let branch_terms = List.tl_exn arguments in
        let datatype =
          match from_unification_typ (Unification.typ scrutinee_term) with
          | TDatatype x -> x
          | _ -> failwith "ill-typed scrutinee"
        in
        let tags =
          datatype
          |> String.Map.find_exn sigma
          |> List.map ~f:(fun (tag, _) -> tag)
          |> List.sort ~compare:String.compare
        in
        EMatch
          ( from_unification_term sigma scrutinee_term
          , List.map2_exn tags branch_terms ~f:(fun tag t ->
                match t with
                | Abstraction (x, _, body) ->
                    (tag, (x, from_unification_term sigma body))
                | _ -> failwith "malformatted match") )
    | Constant (n, _) -> EInt (Int.of_string n)
  in
  Exp.build_abs
    (List.map ~f:(fun (x, tau) -> (x, from_unification_typ tau)) heading)
    body
