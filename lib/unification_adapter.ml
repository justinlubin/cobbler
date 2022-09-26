open Core
open Lang
open Unification

(* Types *)

let rec to_unification_typ : Lang.typ -> Unification.typ = function
  | TUnit -> Elementary TUnit
  | TInt -> Elementary TInt
  | TDatatype x -> Elementary (TDatatype x)
  | TProd (left, right) -> Elementary (TProd (left, right))
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

let embed_name : string -> string -> string =
 fun prefix metadata -> Util.gensym prefix ^ "$" ^ metadata

let unembed_name : string -> (string * string) option =
 fun name ->
  match String.split ~on:'$' name with
  | [ gensymed_prefix; metadata ] ->
      Some (Util.ungensym gensymed_prefix, metadata)
  | _ -> None

let rec embed
    :  Lang.datatype_env -> String.Set.t -> Lang.typ_env -> string -> string
    -> Lang.typ -> Lang.exp list -> Unification.term
  =
 fun sigma stdlib gamma prefix metadata result_type arguments ->
  let head = embed_name prefix metadata in
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
        List.map (sort_tags branches) ~f:(fun (tag, (arg_name, rhs)) ->
            EAbs
              ( arg_name
              , snd (Option.value_exn (Type_system.ctor_typ sigma tag))
              , rhs ))
      in
      embed sigma stdlib gamma "match" "" result_type arguments
  | ECtor (tag, arg) ->
      let dt, _ = Option.value_exn (Type_system.ctor_typ sigma tag) in
      embed sigma stdlib gamma "ctor" tag (TDatatype dt) [ arg ]
  | EPair (e1, e2) ->
      let tau1 = Type_system.infer sigma gamma e1 in
      let tau2 = Type_system.infer sigma gamma e2 in
      embed sigma stdlib gamma "pair" "" (TProd (tau1, tau2)) [ e1; e2 ]
  | EFst arg ->
      (match Type_system.infer sigma gamma arg with
      | TProd (result_type, _) ->
          embed sigma stdlib gamma "fst" "" result_type [ arg ]
      | _ -> failwith "ill-typed fst")
  | ESnd arg ->
      (match Type_system.infer sigma gamma arg with
      | TProd (_, result_type) ->
          embed sigma stdlib gamma "snd" "" result_type [ arg ]
      | _ -> failwith "ill-typed snd")
  | EUnit -> embed sigma stdlib gamma "unit" "" TUnit []
  | EInt n -> embed sigma stdlib gamma "int" (Int.to_string n) TInt []
  | EHole (name, typ) ->
      Atom (Variable (embed_name "??" name, to_unification_typ typ))

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
    | Variable (x, tau) ->
        (match unembed_name x with
        | Some ("??", name) ->
            build_arguments (EHole (name, from_unification_typ tau))
        | _ -> build_arguments (EVar x))
    | Constant (x, _) ->
        (match unembed_name x with
        | Some ("match", "") ->
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
              |> sort_tags
              |> List.map ~f:(fun (tag, _) -> tag)
            in
            EMatch
              ( from_unification_term sigma scrutinee_term
              , List.map2_exn tags branch_terms ~f:(fun tag t ->
                    match t with
                    | Abstraction (x, _, body) ->
                        (tag, (x, from_unification_term sigma body))
                    | _ -> failwith "malformatted match") )
        | Some ("ctor", tag) ->
            ECtor (tag, from_unification_term sigma (List.hd_exn arguments))
        | Some ("pair", "") ->
            EPair
              ( from_unification_term sigma (List.nth_exn arguments 0)
              , from_unification_term sigma (List.nth_exn arguments 1) )
        | Some ("fst", "") ->
            EFst (from_unification_term sigma (List.hd_exn arguments))
        | Some ("snd", "") ->
            ESnd (from_unification_term sigma (List.hd_exn arguments))
        | Some ("unit", "") -> EUnit
        | Some ("int", n) -> EInt (Int.of_string n)
        | _ -> build_arguments (EVar x))
  in
  Exp.build_abs
    (List.map ~f:(fun (x, tau) -> (x, from_unification_typ tau)) heading)
    body
