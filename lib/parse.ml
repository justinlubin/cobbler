open Core
open Lang

let is_constructor : string -> bool =
 fun s -> Char.is_uppercase (String.get s 0)

let rec typ_of_sexp : Sexp.t -> typ = function
  | Sexp.Atom x ->
      if String.equal x "->"
      then failwith "cannot use arrow (->) as type name"
      else TPlaceholder x
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
      if String.is_prefix ~prefix:"__" x
      then failwith "cannot use name starting with double underscore"
      else if String.equal x "->"
      then failwith "cannot use arrow (->) as expression name"
      else (
        try EInt (Int.of_string x) with
        | _ -> EVar x)
  | Sexp.List [ Sexp.Atom "lambda"; Sexp.Atom param; body ] ->
      EAbs (param, exp_of_sexp body)
  | Sexp.List (Sexp.Atom "match" :: scrutinee :: branches) ->
      EMatch (exp_of_sexp scrutinee, List.map ~f:branch_of_sexp branches)
  | Sexp.List [ Sexp.Atom head; arg ] when is_constructor head ->
      ECtor (head, exp_of_sexp arg)
  | Sexp.List (head :: args) ->
      List.fold_left args ~init:(exp_of_sexp head) ~f:(fun acc arg ->
          EApp (acc, exp_of_sexp arg))
  | _ -> failwith "malformed expression"

let definitions : string -> typ_env * env =
 fun text ->
  let typ_env_list, env_all_list =
    text
    |> Parsexp.Many.parse_string_exn
    |> List.map ~f:(fun sexp ->
           match sexp with
           | Sexp.List
               [ Sexp.Atom "define"; Sexp.Atom lhs; Sexp.Atom ":"; typ; rhs ] ->
               ((lhs, typ_of_sexp typ), (lhs, exp_of_sexp rhs))
           | _ -> failwith "malformed top-level definition")
    |> List.unzip
  in
  ( Map.of_alist_exn (module String) typ_env_list
  , Map.of_alist_exn (module String) env_all_list )

let exp : string -> exp =
 fun text -> text |> Parsexp.Single.parse_string_exn |> exp_of_sexp

let extract : id -> typ_env * env -> typ_env * env * typ * exp =
 fun name (gamma, env) ->
  ( Map.remove gamma name
  , Map.remove env name
  , Map.find_exn gamma name
  , Map.find_exn env name )

let extract_main_body
    : typ_env * env -> typ_env * env * (id * typ) list * typ * exp
  =
 fun defs ->
  let gamma', env', main_typ, main_exp = extract "main" defs in
  let domain, codomain = Lang_util.decompose_arrow main_typ in
  ( gamma'
  , env'
  , List.zip_exn (Lang_util.params main_exp) domain
  , codomain
  , main_exp )
