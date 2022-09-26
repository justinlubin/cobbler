open Core
open Lang

let is_constructor : string -> bool =
 fun s -> Char.is_uppercase (String.get s 0)

let is_datatype : string -> bool = fun s -> Char.is_uppercase (String.get s 0)
let is_variable : string -> bool = fun s -> Char.is_lowercase (String.get s 0)

let rec typ_of_sexp : Sexp.t -> typ = function
  | Sexp.Atom "Unit" -> TUnit
  | Sexp.Atom "Int" -> TInt
  | Sexp.Atom x ->
      if is_datatype x
      then TDatatype x
      else failwith (sprintf "unknown atom type '%s'" x)
  | Sexp.List [ domain; Sexp.Atom "*"; range ] ->
      TProd (typ_of_sexp domain, typ_of_sexp range)
  | Sexp.List [ domain; Sexp.Atom "->"; range ] ->
      TArr (typ_of_sexp domain, typ_of_sexp range)
  | Sexp.List _ ->
      failwith "arrow type must have exactly domain, arrow (->), and range"

let rec branch_of_sexp : Sexp.t -> branch = function
  | Sexp.List [ Sexp.Atom ctor_name; Sexp.Atom arg_name; Sexp.Atom "->"; rhs ]
    when is_constructor ctor_name -> (ctor_name, (arg_name, exp_of_sexp rhs))
  | _ -> failwith "malformed branch"

and exp_of_sexp : Sexp.t -> exp = function
  | Sexp.Atom x ->
      (try EInt (Int.of_string x) with
      | _ ->
          if is_variable x
          then EVar x
          else failwith (sprintf "unknown atom expression '%s'" x))
  | Sexp.List [] -> EUnit
  | Sexp.List [ Sexp.Atom "??"; Sexp.Atom name; tau ] ->
      EHole (name, typ_of_sexp tau)
  | Sexp.List [ left; Sexp.Atom ","; right ] ->
      EPair (exp_of_sexp left, exp_of_sexp right)
  | Sexp.List [ Sexp.Atom "fst"; arg ] -> EFst (exp_of_sexp arg)
  | Sexp.List [ Sexp.Atom "snd"; arg ] -> ESnd (exp_of_sexp arg)
  | Sexp.List [ Sexp.Atom "lambda"; Sexp.Atom param; tau; body ] ->
      EAbs (param, typ_of_sexp tau, exp_of_sexp body)
  | Sexp.List (Sexp.Atom "match" :: scrutinee :: branches) ->
      EMatch (exp_of_sexp scrutinee, List.map ~f:branch_of_sexp branches)
  | Sexp.List [ Sexp.Atom head; arg ] when is_constructor head ->
      ECtor (head, exp_of_sexp arg)
  | Sexp.List (head :: args) ->
      Exp.build_app (exp_of_sexp head) (List.map ~f:exp_of_sexp args)

let variant_of_sexp : Sexp.t -> string * typ = function
  | Sexp.List [ Sexp.Atom tag; arg_typ ] -> (tag, typ_of_sexp arg_typ)
  | _ -> failwith "malformatted datatype variant"

let definitions : string -> datatype_env * typ_env * env =
 fun text ->
  text
  |> Parsexp.Many.parse_string_exn
  |> List.fold_left
       ~init:(String.Map.empty, String.Map.empty, String.Map.empty)
       ~f:(fun (sigma, gamma, env) sexp ->
         match sexp with
         | Sexp.List (Sexp.Atom "type" :: Sexp.Atom dt :: variants) ->
             ( String.Map.add_exn
                 sigma
                 ~key:dt
                 ~data:(List.map ~f:variant_of_sexp variants)
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
