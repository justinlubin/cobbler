open Core
open Lang

let clean_name : string -> string =
 fun s ->
  s
  |> String.substr_replace_all ~pattern:"#" ~with_:""
  |> String.lstrip ~drop:(Char.equal '_')

let rec prettify_exp : exp -> exp =
 fun e ->
  match e with
  | EVar x ->
      (match String.chop_suffix ~suffix:"____CBR" x with
      | Some left ->
          (match String.chop_prefix ~prefix:"maybe_" left with
          | Some mid -> EVar (sprintf "Maybe.%s" (clean_name mid))
          | None -> EVar (clean_name x))
      | None -> EVar (clean_name x))
  | EApp (e1, e2) -> EApp (prettify_exp e1, prettify_exp e2)
  | EAbs (x, body) -> EAbs (clean_name x, prettify_exp body)
  | EMatch (scrutinee, branches) ->
      EMatch (prettify_exp scrutinee, Exp.map_branches ~f:prettify_exp branches)
  | ECtor (ctor, args) ->
      ECtor
        ( (match ctor with
          | "Cons" -> "(::)"
          | "Nil" -> "[]"
          | _ -> ctor)
        , List.map ~f:prettify_exp args )
  | EBase b -> EBase b
  | EHole (name, tau) -> EHole (name, tau)
  | ERScheme (rs, dt, args) -> ERScheme (rs, dt, List.map ~f:prettify_exp args)

let rec prettify_typ : typ -> typ =
 fun t ->
  match t with
  | TBase b -> TBase b
  | TVar x -> TVar (clean_name x)
  | TDatatype (dt, args) -> TDatatype (dt, List.map ~f:prettify_typ args)
  | TArr (t1, t2) -> TArr (prettify_typ t1, prettify_typ t2)

let rec typ' : typ -> string =
 fun t ->
  match t with
  | TBase BTInt -> "Int"
  | TBase BTString -> "String"
  | TBase BTFloat -> "Float"
  | TVar x -> x
  | TDatatype (dt, args) ->
      sprintf
        "(%s%s)"
        dt
        (args |> List.map ~f:(fun a -> " " ^ typ' a) |> String.concat)
  | TArr (t1, t2) -> sprintf "(%s -> %s)" (typ' t1) (typ' t2)

let typ : typ -> string = fun t -> typ' (prettify_typ t)

let rec exp'' : int -> exp -> string =
 fun depth e ->
  let indent = "\n" ^ String.init (4 * (depth + 1)) ~f:(fun _ -> ' ') in
  match e with
  | EVar x -> sprintf "(%s)" x
  | EApp (e1, e2) -> sprintf "(%s %s)" (exp'' depth e1) (exp'' depth e2)
  | EAbs (x, body) -> sprintf "(\\%s -> %s)" x (exp'' depth body)
  | EMatch (scrutinee, branches) ->
      sprintf
        "(case %s of%s)"
        (exp'' depth scrutinee)
        (branches
        |> List.map ~f:(fun (ctor, (params, rhs)) ->
               sprintf
                 "%s%s%s -> %s"
                 indent
                 ctor
                 (params |> List.map ~f:(fun p -> " " ^ p) |> String.concat)
                 (exp'' depth rhs))
        |> String.concat)
  | ECtor (ctor, args) ->
      sprintf
        "(%s%s)"
        ctor
        (args |> List.map ~f:(fun a -> " " ^ exp'' depth a) |> String.concat)
  | EBase (BEInt n) -> string_of_int n
  | EBase (BEString s) -> sprintf "\"%s\"" s
  | EBase (BEFloat f) -> string_of_float f
  | EHole (_, _) -> failwith "Cannot unparse hole"
  | ERScheme (RSCata, _, args) -> exp'' depth (Exp.build_app (EVar "cata") args)

let exp' : int -> exp -> string = fun depth e -> exp'' depth (prettify_exp e)
let exp : exp -> string = fun e -> exp' 0 e

let definition : string -> typ_scheme -> exp -> string =
 fun name (_, t) rhs ->
  let params, inner_rhs = Exp.decompose_abs rhs in
  sprintf
    "%s : %s\n%s%s = %s"
    name
    (typ t)
    name
    (params |> List.map ~f:(fun p -> " " ^ p) |> String.concat)
    (exp' 1 inner_rhs)
