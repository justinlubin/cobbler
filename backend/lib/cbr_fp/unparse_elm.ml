open Core
open Lang

let escape : string -> string =
 fun s ->
  s
  |> String.substr_replace_all ~pattern:"\\" ~with_:"\\\\"
  |> String.substr_replace_all ~pattern:"\n" ~with_:"\\n"
  |> String.substr_replace_all ~pattern:"\r" ~with_:"\\r"
  |> String.substr_replace_all ~pattern:"\t" ~with_:"\\t"
  |> String.substr_replace_all ~pattern:"\"" ~with_:"\\\""

let infix_chars : Char.Set.t =
  Char.Set.of_list
    [ '+'
    ; '-'
    ; '/'
    ; '*'
    ; '='
    ; '.'
    ; '<'
    ; '>'
    ; ':'
    ; '&'
    ; '|'
    ; '^'
    ; '?'
    ; '%'
    ; '!'
    ]

let is_infix : string -> bool =
 fun s -> String.for_all ~f:(Set.mem infix_chars) s

let pretty_ctor : string -> string = function
  | "Basics.True" -> "True"
  | "Basics.False" -> "False"
  | "Maybe.Just" -> "Just"
  | "Maybe.Nothing" -> "Nothing"
  | "Result.Ok" -> "Ok"
  | "Result.Err" -> "Err"
  | s -> s

let clean_name : string -> string =
 fun s ->
  s
  |> String.substr_replace_all ~pattern:"#" ~with_:""
  |> String.lstrip ~drop:(Char.equal '_')

let clean_pat : string -> string = function
  | s when String.is_prefix ~prefix:"__wildcard#" s -> "_"
  | s -> s

let rec list_literal : exp -> exp -> exp list option =
 fun hd tl ->
  match tl with
  | ECtor ("Nil", []) -> Some [ hd ]
  | ECtor ("Cons", [ hd'; tl' ]) ->
      Option.map ~f:(fun es -> hd :: es) (list_literal hd' tl')
  | _ -> None

let prettify_exp : datatype_env -> exp -> exp =
 fun sigma ->
  let rec recur e =
    match e with
    | EVar "append____CBR_builtin" -> EVar "++"
    | EVar "or____CBR_inline" -> EVar "||"
    | EVar "and____CBR_inline" -> EVar "&&"
    | EVar x ->
        (match String.chop_suffix ~suffix:"____CBR" x with
        | Some left ->
            (match String.chop_prefix ~prefix:"basics_" left with
            | Some mid -> EVar (sprintf "$%s" (clean_name mid))
            | None ->
                (match String.chop_prefix ~prefix:"maybe_" left with
                | Some mid -> EVar (sprintf "$Maybe.%s" (clean_name mid))
                | None ->
                    (match String.chop_prefix ~prefix:"result_" left with
                    | Some mid -> EVar (sprintf "$Result.%s" (clean_name mid))
                    | None ->
                        (match String.chop_prefix ~prefix:"list_" left with
                        | Some mid -> EVar (sprintf "$List.%s" (clean_name mid))
                        | None -> EVar (clean_name x)))))
        | None -> EVar (clean_name x))
    | EApp (ERScheme (RSCata RSCataNonrecursive, dt, args), scrutinee) ->
        (match Map.find sigma dt with
        | Some (_, variants) ->
            let branches =
              List.map2_exn variants args ~f:(fun (ctor_name, _) cata_arg ->
                  (ctor_name, Exp.decompose_abs cata_arg))
            in
            recur (EMatch (scrutinee, branches))
        | None ->
            ERScheme (RSCata RSCataNonrecursive, dt, List.map ~f:recur args))
    | EApp (e1, e2) -> EApp (recur e1, recur e2)
    | EAbs (x, body) ->
        let x = clean_name x in
        (match recur body with
        | EApp (f, EVar x')
          when String.equal x x' && not (Set.mem (Exp.free_variables f) x) -> f
        | body when not (Set.mem (Exp.free_variables body) x) -> EAbs ("_", body)
        | body -> EAbs (x, body))
    | EMatch (scrutinee, branches) ->
        (match branches with
        | [ ("True", ([], ECtor ("True", []))); ("False", ([], e')) ] ->
            recur (Exp.build_app (EVar "||") [ scrutinee; e' ])
        | [ ("True", ([], e')); ("False", ([], ECtor ("False", []))) ] ->
            recur (Exp.build_app (EVar "&&") [ scrutinee; e' ])
        | _ -> EMatch (recur scrutinee, Exp.map_branches ~f:recur branches))
    | ECtor ("Nil", []) -> ECtor ("[]", [])
    | ECtor ("Cons", [ hd; tl ]) ->
        (match list_literal hd tl with
        | Some elements -> ECtor ("::Literal", List.map ~f:recur elements)
        | None -> ECtor ("::", [ recur hd; recur tl ]))
    | ECtor (ctor, args) -> ECtor (ctor, List.map ~f:recur args)
    | EBase b -> EBase b
    | EHole (name, tau) -> EHole (name, tau)
    | ERScheme (rs, dt, args) -> ERScheme (rs, dt, List.map ~f:recur args)
  in
  recur

let rec prettify_typ : typ -> typ =
 fun t ->
  match t with
  | TBase b -> TBase b
  | TVar x -> TVar (clean_name x)
  | TDatatype (dt, args) -> TDatatype (dt, List.map ~f:prettify_typ args)
  | TArr (t1, t2) -> TArr (prettify_typ t1, prettify_typ t2)

let rec typ' : typ -> string =
 fun t ->
  let dom, cod = Typ.decompose_arr t in
  let cod_string =
    match cod with
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
  in
  "("
  ^ (dom |> List.map ~f:(fun d -> typ' d ^ " -> ") |> String.concat ~sep:"")
  ^ cod_string
  ^ ")"

let typ : typ -> string = fun t -> typ' (prettify_typ t)

let rec exp'' : ?in_pipeline:bool -> int -> exp -> string =
 fun ?(in_pipeline = false) depth e ->
  let indent = "\n" ^ String.init (4 * (depth + 1)) ~f:(fun _ -> ' ') in
  match Exp.decompose_app e with
  | EVar f, [ arg ] when is_infix f -> sprintf "((%s) %s)" f (exp'' depth arg)
  | EVar f, [ left; right ] when is_infix f ->
      sprintf "(%s %s %s)" (exp'' depth left) f (exp'' depth right)
  | EVar f, (_ :: _ as args) when Char.equal (String.get f 0) '$' ->
      let nonlast_args = List.drop_last_exn args in
      let last_arg = List.last_exn args in
      let left, right = if in_pipeline then ("", "") else ("(", ")") in
      sprintf
        "%s%s%s|> %s %s%s"
        left
        (exp'' ~in_pipeline:true depth last_arg)
        indent
        (String.drop_prefix f 1)
        (String.concat ~sep:" " (List.map ~f:(exp'' (depth + 1)) nonlast_args))
        right
  | _ ->
      (match e with
      | EVar x -> sprintf "(%s)" x
      | EApp (e1, e2) -> sprintf "(%s %s)" (exp'' depth e1) (exp'' depth e2)
      | EAbs (x, body) ->
          sprintf "(\\%s -> %s)" (clean_pat x) (exp'' depth body)
      | EMatch (scrutinee, branches) ->
          sprintf
            "(case %s of%s)"
            (exp'' depth scrutinee)
            (branches
            |> List.map ~f:(fun (ctor, (params, rhs)) ->
                   match (ctor, params) with
                   | "Nil", [] -> sprintf "%s[] -> %s" indent (exp'' depth rhs)
                   | "Cons", [ hd; tl ] ->
                       sprintf
                         "%s%s :: %s -> %s"
                         indent
                         (clean_pat hd)
                         (clean_pat tl)
                         (exp'' depth rhs)
                   | _, _ ->
                       sprintf
                         "%s%s%s -> %s"
                         indent
                         (pretty_ctor ctor)
                         (params
                         |> List.map ~f:(fun p -> " " ^ clean_pat p)
                         |> String.concat)
                         (exp'' depth rhs))
            |> String.concat)
      | ECtor ("::Literal", elements) ->
          let inner =
            elements
            |> List.map ~f:(exp'' (depth + 1))
            |> String.concat ~sep:", "
          in
          "["
          ^ (if String.length inner > 40 then "\n" ^ indent else "")
          ^ inner
          ^ "]"
      | ECtor (ctor, [ left; right ]) when is_infix ctor ->
          sprintf
            "(%s %s %s)"
            (exp'' depth left)
            (pretty_ctor ctor)
            (exp'' depth right)
      | ECtor (ctor, args) ->
          sprintf
            "(%s%s)"
            ctor
            (args |> List.map ~f:(fun a -> " " ^ exp'' depth a) |> String.concat)
      | EBase (BEInt n) -> string_of_int n
      | EBase (BEString s) -> sprintf "\"%s\"" (escape s)
      | EBase (BEFloat f) -> string_of_float f
      | EHole (_, _) -> failwith "Cannot unparse hole"
      | ERScheme _ -> failwith "Cannot unparse recursion scheme")

let exp' : datatype_env -> int -> exp -> string =
 fun sigma depth e -> exp'' depth (prettify_exp sigma (Exp.freshen_univars e))

let exp : datatype_env -> exp -> string = fun sigma e -> exp' sigma 0 e

let definition : datatype_env -> string -> typ_scheme -> exp -> string =
 fun sigma name (_, t) rhs ->
  let params, inner_rhs = Exp.decompose_abs rhs in
  sprintf
    "%s : %s\n%s%s = %s"
    name
    (typ t)
    name
    (params |> List.map ~f:(fun p -> " " ^ clean_pat p) |> String.concat)
    (exp' sigma 1 inner_rhs)
