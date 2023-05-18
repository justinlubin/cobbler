open Core
open Lang

let is_type_var : string -> bool = fun s -> Char.is_lowercase (String.get s 0)

let is_constructor : string -> bool =
 fun s -> Char.is_uppercase (String.get s 0)

let is_datatype : string -> bool = fun s -> Char.is_uppercase (String.get s 0)
let is_variable : string -> bool = fun s -> Char.is_lowercase (String.get s 0)

let rec typ_of_sexp : Sexp.t -> typ =
 fun s ->
  match s with
  | Sexp.Atom "Int" -> TInt
  | Sexp.Atom x when is_type_var x -> TVar x
  | Sexp.List [ domain; Sexp.Atom "->"; range ] ->
      TArr (typ_of_sexp domain, typ_of_sexp range)
  | Sexp.List (Sexp.Atom head :: tail) ->
      if is_datatype head
      then TDatatype (head, List.map ~f:typ_of_sexp tail)
      else failwith (sprintf "unknown atom type '%s'" head)
  | _ -> failwith (sprintf "unknown type: %s" ([%show: Sexp.t] s))

let branch_pattern_of_sexp : Sexp.t -> string = function
  | Sexp.Atom x -> x
  | _ -> failwith "malformatted branch pattern"

let rec branch_of_sexp : Sexp.t -> branch =
 fun s ->
  match s with
  | Sexp.List
      [ Sexp.List (Sexp.Atom ctor_name :: arg_names); Sexp.Atom "->"; rhs ]
    when is_constructor ctor_name ->
      ( ctor_name
      , (List.map ~f:branch_pattern_of_sexp arg_names, exp_of_sexp rhs) )
  | _ -> failwith (sprintf "malformed branch: %s" ([%show: Sexp.t] s))

and exp_of_sexp : Sexp.t -> exp = function
  | Sexp.Atom x ->
      (try EInt (Int.of_string x) with
      | _ ->
          if is_variable x
          then EVar x
          else failwith (sprintf "unknown atom expression '%s'" x))
  | Sexp.List [] -> failwith "unit syntax not supported"
  | Sexp.List [ Sexp.Atom "??"; Sexp.Atom name; tau ] ->
      EHole (name, typ_of_sexp tau)
  | Sexp.List [ Sexp.Atom "lambda"; Sexp.Atom param; tau; body ] ->
      EAbs (param, typ_of_sexp tau, exp_of_sexp body)
  | Sexp.List [ Sexp.Atom "list_foldr"; b; f; arg ] ->
      EApp
        (ERScheme (RListFoldr (exp_of_sexp b, exp_of_sexp f)), exp_of_sexp arg)
  | Sexp.List (Sexp.Atom "match" :: scrutinee :: branches) ->
      EMatch (exp_of_sexp scrutinee, List.map ~f:branch_of_sexp branches)
  | Sexp.List (Sexp.Atom head :: args) when is_constructor head ->
      ECtor (head, List.map ~f:exp_of_sexp args)
  | Sexp.List (head :: args) ->
      Exp.build_app (exp_of_sexp head) (List.map ~f:exp_of_sexp args)

let variant_of_sexp : Sexp.t -> string * typ list = function
  | Sexp.List (Sexp.Atom tag :: arg_typs) ->
      (tag, List.map ~f:typ_of_sexp arg_typs)
  | _ -> failwith "malformatted datatype variant"

let parameter_of_sexp : Sexp.t -> string = function
  | Sexp.Atom param -> param
  | _ -> failwith "malformatted datatype parameter"

let definitions : string -> datatype_env * typ_env * env =
 fun text ->
  text
  |> Parsexp.Many.parse_string_exn
  |> List.fold_left
       ~init:(String.Map.empty, String.Map.empty, String.Map.empty)
       ~f:(fun (sigma, gamma, env) sexp ->
         match sexp with
         | Sexp.List
             (Sexp.Atom "type"
             :: Sexp.List (Sexp.Atom dt :: parameters)
             :: variants) ->
             ( String.Map.add_exn
                 sigma
                 ~key:dt
                 ~data:
                   ( List.map ~f:parameter_of_sexp parameters
                   , List.map ~f:variant_of_sexp variants )
             , gamma
             , env )
         | Sexp.List
             [ Sexp.Atom "define"; Sexp.Atom lhs; Sexp.Atom ":"; typ; rhs ] ->
             ( sigma
             , String.Map.add_exn gamma ~key:lhs ~data:(typ_of_sexp typ)
             , String.Map.add_exn env ~key:lhs ~data:(exp_of_sexp rhs) )
         | _ ->
             failwith
               (sprintf
                  "malformed top-level definition: %s"
                  (Sexp.to_string sexp)))

let exp : string -> exp =
 fun text -> text |> Parsexp.Single.parse_string_exn |> exp_of_sexp
