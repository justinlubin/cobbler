open Core
open Lang

let is_constructor : string -> bool =
 fun s -> Char.is_uppercase (String.get s 0)

let rec branch_of_sexp : Sexp.t -> branch = function
  | Sexp.List [ Sexp.Atom ctor_name; Sexp.Atom arg_name; Sexp.Atom "->"; rhs ]
    when is_constructor ctor_name -> (ctor_name, (arg_name, exp_of_sexp rhs))
  | _ -> failwith "malformed branch"

and exp_of_sexp : Sexp.t -> exp = function
  | Sexp.Atom x ->
      if String.is_prefix ~prefix:"__" x
      then failwith "cannot use name starting with double underscore"
      else EVar x
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

let program : string -> env * exp =
 fun text ->
  let definitions =
    text
    |> Parsexp.Many.parse_string_exn
    |> List.map ~f:(fun sexp ->
           match sexp with
           | Sexp.List [ Sexp.Atom "define"; Sexp.Atom lhs; rhs ] ->
               (lhs, exp_of_sexp rhs)
           | _ -> failwith "malformed top-level definition")
    |> Map.of_alist_exn (module String)
  in
  let main = Map.find_exn definitions "main" in
  (Map.remove definitions "main", main)
