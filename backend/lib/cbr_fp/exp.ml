open Core
open Lang

module T = struct
  type t = exp

  let compare = compare_exp
  let sexp_of_t = sexp_of_exp
end

include T
include Comparator.Make (T)

let rec show_single : exp -> string =
 fun e ->
  match e with
  | EVar id -> id
  | EApp (head, arg) -> sprintf "(%s %s)" (show_single head) (show_single arg)
  | EAbs (param, body) -> sprintf "(lambda %s %s)" param (show_single body)
  | EMatch (scrutinee, branches) ->
      sprintf
        "(match %s %s)"
        (show_single scrutinee)
        (String.concat
           ~sep:" "
           (List.map
              ~f:(fun (ctor_name, (arg_names, rhs)) ->
                sprintf
                  "((%s%s) -> %s)"
                  ctor_name
                  (List.map ~f:(fun a -> " " ^ a) arg_names |> String.concat)
                  (show_single rhs))
              branches))
  | ECtor (ctor_name, args) ->
      sprintf
        "(%s%s)"
        ctor_name
        (List.map ~f:(fun a -> " " ^ show_single a) args |> String.concat)
  | EBase (BEInt n) -> string_of_int n
  | EBase (BEFloat f) -> string_of_float f
  | EBase (BEString s) -> sprintf "\"%s\"" s
  | EHole (name, typ) -> sprintf "(?? %s %s)" name (Typ.show typ)
  | ERScheme (RSCata _, dt, args) ->
      sprintf
        "(cata %s%s)"
        dt
        (args |> List.map ~f:(fun a -> " " ^ show_single a) |> String.concat)

let rec show_multi : int -> exp -> string =
 fun depth e ->
  let single_indent = "  " in
  let indent = String.concat (List.init depth ~f:(fun _ -> single_indent)) in
  indent
  ^
  match e with
  | EVar id -> id
  | EApp (head, arg) -> sprintf "(%s %s)" (show_single head) (show_single arg)
  | EAbs (param, body) ->
      sprintf "(lambda %s\n%s)" param (show_multi (depth + 1) body)
  | EMatch (scrutinee, branches) ->
      sprintf
        "(match %s\n%s)"
        (show_single scrutinee)
        (String.concat
           ~sep:"\n"
           (List.map
              ~f:(fun (ctor_name, (arg_names, rhs)) ->
                sprintf
                  "%s%s((%s%s) ->\n%s)"
                  indent
                  single_indent
                  ctor_name
                  (List.map ~f:(fun a -> " " ^ a) arg_names |> String.concat)
                  (show_multi (depth + 2) rhs))
              branches))
  | ECtor (ctor_name, args) ->
      sprintf
        "(%s%s)"
        ctor_name
        (List.map ~f:(fun a -> " " ^ show_single a) args |> String.concat)
  | EBase (BEInt n) -> string_of_int n
  | EBase (BEFloat f) -> string_of_float f
  | EBase (BEString s) -> sprintf "\"%s\"" s
  | EHole (name, typ) -> sprintf "(?? %s %s)" name (Typ.show typ)
  | ERScheme (RSCata _, dt, args) ->
      sprintf
        "(cata %s%s)"
        dt
        (args |> List.map ~f:(fun a -> " " ^ show_single a) |> String.concat)

let map_branches : branch list -> f:(exp -> exp) -> branch list =
 fun branches ~f ->
  List.map branches ~f:(fun (ctor_name, (arg_name, rhs)) ->
      (ctor_name, (arg_name, f rhs)))

let decompose_abs : exp -> string list * exp =
 fun e ->
  let rec decompose_abs' acc = function
    | EAbs (param, body) -> decompose_abs' (param :: acc) body
    | rest -> (List.rev acc, rest)
  in
  decompose_abs' [] e

let decompose_app : exp -> exp * exp list =
 fun t ->
  let rec decompose_app' acc = function
    | EApp (t1, t2) -> decompose_app' (t2 :: acc) t1
    | rest -> (rest, acc)
  in
  decompose_app' [] t

let build_abs : string list -> exp -> exp =
 fun xs t -> List.fold_right xs ~init:t ~f:(fun x acc -> EAbs (x, acc))

let build_app : exp -> exp list -> exp =
 fun head args ->
  List.fold_left args ~init:head ~f:(fun acc arg -> EApp (acc, arg))

let rec all_variables : exp -> String.Set.t = function
  | EVar x -> String.Set.singleton x
  | EApp (head, arg) -> Set.union (all_variables head) (all_variables arg)
  | EAbs (param, body) -> Set.add (all_variables body) param
  | EMatch (scrutinee, branches) ->
      String.Set.union_list
        (all_variables scrutinee
        :: List.map
             ~f:(fun (_, (arg_names, rhs)) ->
               Set.union (all_variables rhs) (String.Set.of_list arg_names))
             branches)
  | ECtor (_, arg) -> String.Set.union_list (List.map ~f:all_variables arg)
  | ERScheme (_, _, args) ->
      args |> List.map ~f:all_variables |> String.Set.union_list
  | EBase _ | EHole (_, _) -> String.Set.empty

let rec free_variables : exp -> String.Set.t = function
  | EVar x -> String.Set.singleton x
  | EApp (head, arg) -> Set.union (free_variables head) (free_variables arg)
  | EAbs (param, body) -> Set.remove (free_variables body) param
  | EMatch (scrutinee, branches) ->
      String.Set.union_list
        (free_variables scrutinee
        :: List.map
             ~f:(fun (_, (arg_names, rhs)) ->
               Set.diff (free_variables rhs) (String.Set.of_list arg_names))
             branches)
  | ECtor (_, arg) -> String.Set.union_list (List.map ~f:free_variables arg)
  | ERScheme (_, _, args) ->
      args |> List.map ~f:free_variables |> String.Set.union_list
  | EBase _ | EHole (_, _) -> String.Set.empty

let replace : string * string -> exp -> exp =
 fun (lhs, rhs) e ->
  let rec replace' = function
    | EVar x -> if String.equal lhs x then EVar rhs else EVar x
    | EApp (head, arg) -> EApp (replace' head, replace' arg)
    | EAbs (param, body) ->
        if String.equal lhs param
        then EAbs (rhs, replace' body)
        else EAbs (param, replace' body)
    | EMatch (scrutinee, branches) ->
        EMatch
          ( replace' scrutinee
          , List.map branches ~f:(fun (ctor_name, (arg_names, branch_rhs)) ->
                ( ctor_name
                , ( List.map
                      ~f:(fun arg_name ->
                        if String.equal lhs arg_name then rhs else arg_name)
                      arg_names
                  , replace' branch_rhs ) )) )
    | ECtor (ctor_name, args) -> ECtor (ctor_name, List.map ~f:replace' args)
    | EBase b -> EBase b
    | EHole (name, typ) -> EHole (name, typ)
    | ERScheme (rs, dt, args) -> ERScheme (rs, dt, List.map ~f:replace' args)
  in
  replace' e

let replace_all : (string * string) list -> exp -> exp =
 fun subs e -> List.fold_left ~f:(fun acc sub -> replace sub acc) ~init:e subs

let gensym_prefix : string = "var"

let substitute : string * exp -> exp -> exp =
 fun (lhs, rhs) e ->
  let rhs_fv = free_variables rhs in
  let rec substitute' = function
    | EVar x -> if String.equal lhs x then rhs else EVar x
    | EApp (head, arg) -> EApp (substitute' head, substitute' arg)
    | EAbs (param, body) ->
        if String.equal lhs param
        then EAbs (param, body)
        else if not (Set.mem rhs_fv param)
        then EAbs (param, substitute' body)
        else (
          let new_param = Util.gensym gensym_prefix in
          EAbs (new_param, substitute' (replace (param, new_param) body)))
    | EMatch (scrutinee, branches) ->
        EMatch
          ( substitute' scrutinee
          , List.map
              ~f:(fun (ctor_name, (arg_names, branch_rhs)) ->
                if List.mem ~equal:[%eq: string] arg_names lhs
                then (ctor_name, (arg_names, branch_rhs))
                else if List.for_all arg_names ~f:(fun arg_name ->
                            not (Set.mem rhs_fv arg_name))
                then (ctor_name, (arg_names, substitute' branch_rhs))
                else (
                  let replacements =
                    List.map
                      ~f:(fun a -> (a, Util.gensym gensym_prefix))
                      arg_names
                  in
                  ( ctor_name
                  , ( List.map ~f:snd replacements
                    , substitute' (replace_all replacements branch_rhs) ) )))
              branches )
    | ECtor (ctor_name, args) -> ECtor (ctor_name, List.map ~f:substitute' args)
    | EBase b -> EBase b
    | EHole (name, typ) -> EHole (name, typ)
    | ERScheme (rs, dt, args) -> ERScheme (rs, dt, List.map ~f:substitute' args)
  in
  substitute' e

let substitute_all : (string * exp) list -> exp -> exp =
 fun subs e ->
  List.fold_left ~f:(fun acc sub -> substitute sub acc) ~init:e subs

let freshen_with : (string -> string) -> exp -> exp =
 fun renamer e ->
  let rec freshen_with' = function
    | EVar x -> EVar x
    | EApp (head, arg) -> EApp (freshen_with' head, freshen_with' arg)
    | EAbs (param, body) ->
        let new_param = renamer param in
        EAbs (new_param, freshen_with' (replace (param, new_param) body))
    | EMatch (scrutinee, branches) ->
        EMatch
          ( freshen_with' scrutinee
          , List.map branches ~f:(fun (ctor_name, (arg_names, rhs)) ->
                let replacements =
                  List.map ~f:(fun a -> (a, renamer a)) arg_names
                in
                ( ctor_name
                , ( List.map ~f:snd replacements
                  , freshen_with' (replace_all replacements rhs) ) )) )
    | ECtor (ctor_name, args) ->
        ECtor (ctor_name, List.map ~f:freshen_with' args)
    | EBase b -> EBase b
    | EHole (name, typ) -> EHole (name, typ)
    | ERScheme (rs, dt, args) ->
        ERScheme (rs, dt, List.map ~f:freshen_with' args)
  in
  freshen_with' e

let freshen : exp -> exp = freshen_with (fun _ -> Util.gensym gensym_prefix)

let alphabet : string list =
  [ "x"
  ; "y"
  ; "z"
  ; "a"
  ; "b"
  ; "c"
  ; "d"
  ; "e"
  ; "f"
  ; "g"
  ; "h"
  ; "i"
  ; "j"
  ; "k"
  ; "l"
  ; "m"
  ; "n"
  ; "o"
  ; "p"
  ; "q"
  ; "r"
  ; "s"
  ; "t"
  ; "u"
  ; "v"
  ; "w"
  ]

let is_univar : string -> bool =
 fun s ->
  match String.chop_prefix ~prefix:"__univar#" s with
  | Some s -> s |> Int.of_string_opt |> Option.is_some
  | None -> false

let freshen_univars : exp -> exp =
 fun e ->
  let all_vars = all_variables e in
  let nice_vars =
    ref
      (List.filter_map
         ~f:(fun x -> if not (Set.mem all_vars x) then Some (x, 0) else None)
         alphabet)
  in
  let fresh_nice_var () =
    match !nice_vars with
    | (hd, n) :: tl ->
        nice_vars := tl @ [ (hd, n + 1) ];
        hd ^ if Int.equal n 0 then "" else string_of_int n
    | _ -> failwith "impossible"
  in
  freshen_with (fun x -> if is_univar x then fresh_nice_var () else x) e

let alpha_normalize : exp -> exp =
 fun e ->
  let suffix = ref (-1) in
  freshen_with
    (fun _ ->
      suffix := !suffix + 1;
      gensym_prefix ^ Int.to_string !suffix)
    e

let alpha_equivalent : exp -> exp -> bool =
 fun e1 e2 -> [%eq: exp] (alpha_normalize e1) (alpha_normalize e2)

let normalize : datatype_env -> exp -> exp =
 fun sigma e ->
  let rec recur = function
    | EVar id -> EVar id
    | EApp (head, arg) ->
        (match (recur head, recur arg) with
        | EAbs (param, body), arg' -> substitute (param, arg') body
        | ERScheme (RSCata ct, dt, cata_args), ECtor (ctor, ctor_args) ->
            let dt_params, ctors = Map.find_exn sigma dt in
            let index, ctor_domain =
              List.find_mapi_exn
                ~f:(fun i (c, d) ->
                  if String.equal c ctor then Some (i, d) else None)
                ctors
            in
            recur
              (build_app
                 (List.nth_exn cata_args index)
                 (List.map2_exn
                    ~f:(fun arg tau ->
                      match tau with
                      | TDatatype (dt', _) when String.equal dt dt' ->
                          recur
                            (EApp (ERScheme (RSCata ct, dt, cata_args), arg))
                      | _ -> arg)
                    ctor_args
                    ctor_domain))
        | head', arg' -> EApp (head', arg'))
    | EAbs (param, body) -> EAbs (param, recur body)
    | EMatch (scrutinee, branches) ->
        (match recur scrutinee with
        | ECtor (ctor_name, args) ->
            let arg_names, rhs =
              List.Assoc.find_exn ~equal:String.equal branches ctor_name
            in
            recur (substitute_all (List.zip_exn arg_names args) rhs)
        | scrutinee' -> EMatch (scrutinee', map_branches ~f:recur branches))
    | ECtor (ctor_name, args) -> ECtor (ctor_name, List.map ~f:recur args)
    | EBase b -> EBase b
    | EHole (name, typ) -> EHole (name, typ)
    | ERScheme (rs, dt, args) -> ERScheme (rs, dt, List.map ~f:recur args)
  in
  recur e

let replace_subexp : old_subexp:exp -> new_subexp:exp -> exp -> exp =
 fun ~old_subexp ~new_subexp e ->
  let rec recurse = function
    | EVar x -> EVar x
    | EApp (head, arg) -> EApp (check_and_replace head, check_and_replace arg)
    | EAbs (param, body) -> EAbs (param, check_and_replace body)
    | EMatch (scrutinee, branches) ->
        EMatch
          ( check_and_replace scrutinee
          , map_branches ~f:check_and_replace branches )
    | ECtor (ctor_name, args) ->
        ECtor (ctor_name, List.map ~f:check_and_replace args)
    | EBase b -> EBase b
    | EHole (name, typ) -> EHole (name, typ)
    | ERScheme (rs, dt, args) ->
        ERScheme (rs, dt, List.map ~f:check_and_replace args)
  and check_and_replace e =
    if [%eq: exp] e old_subexp then new_subexp else recurse e
  in
  check_and_replace e

let fill_holes : (string * exp) list -> exp -> exp =
 fun bindings e ->
  let rec recurse = function
    (* Main case*)
    | EHole (name, typ) ->
        (match List.Assoc.find ~equal:String.equal bindings name with
        | Some rhs -> recurse rhs
        | None -> EHole (name, typ))
    (* Other cases *)
    | EVar x -> EVar x
    | EApp (head, arg) -> EApp (recurse head, recurse arg)
    | EAbs (param, body) -> EAbs (param, recurse body)
    | EMatch (scrutinee, branches) ->
        EMatch (recurse scrutinee, map_branches ~f:recurse branches)
    | ECtor (ctor_name, arg) -> ECtor (ctor_name, List.map ~f:recurse arg)
    | EBase b -> EBase b
    | ERScheme (rs, dt, args) -> ERScheme (rs, dt, List.map ~f:recurse args)
  in
  recurse e

let rec clean : exp -> exp = function
  (* Main cases *)
  (* Eta-equivalence *)
  | EAbs (param, EApp (EVar f, EVar x)) when String.equal x param -> EVar f
  (* Other cases *)
  | EVar x -> EVar x
  | EApp (head, arg) -> EApp (clean head, clean arg)
  | EAbs (param, body) -> EAbs (param, clean body)
  | EMatch (scrutinee, branches) ->
      EMatch (clean scrutinee, map_branches ~f:clean branches)
  | ECtor (ctor_name, arg) -> ECtor (ctor_name, List.map ~f:clean arg)
  | EBase b -> EBase b
  | EHole (name, typ) -> EHole (name, typ)
  | ERScheme (rs, dt, args) -> ERScheme (rs, dt, List.map ~f:clean args)

let apply_type_sub : Typ.sub -> exp -> exp =
 fun subst e ->
  let rec recurse = function
    (* Main case *)
    | EHole (name, typ) -> EHole (name, Typ.apply_sub subst typ)
    (* Other cases *)
    | EVar x -> EVar x
    | EApp (head, arg) -> EApp (recurse head, recurse arg)
    | EAbs (param, body) -> EAbs (param, recurse body)
    | EMatch (scrutinee, branches) ->
        EMatch (recurse scrutinee, map_branches ~f:recurse branches)
    | ECtor (ctor_name, arg) -> ECtor (ctor_name, List.map ~f:recurse arg)
    | EBase b -> EBase b
    | ERScheme (rs, dt, args) -> ERScheme (rs, dt, List.map ~f:recurse args)
  in
  recurse e

let compare_branches : branch -> branch -> int =
 fun (ctor1, _) (ctor2, _) -> String.compare ctor1 ctor2

let pattern_match : reference:exp -> sketch:exp -> (string * exp) list option =
 fun ~reference ~sketch ->
  let var_assert xs ys vmap =
    List.fold2_exn
      ~init:vmap
      ~f:(fun vmap x y ->
        match List.Assoc.find vmap x ~equal:String.equal with
        | Some y' ->
            assert (String.equal y y');
            vmap
        | None -> (x, y) :: vmap)
      xs
      ys
  in
  let rec recurse r s map =
    match (r, s) with
    | EVar rx, EVar sx -> (fst map, var_assert [ rx ] [ sx ] (snd map))
    | EApp (rhead, rarg), EApp (shead, sarg) ->
        map |> recurse rhead shead |> recurse rarg sarg
    | EAbs (rx, rbody), EAbs (sx, sbody) ->
        recurse rbody sbody (fst map, var_assert [ rx ] [ sx ] (snd map))
    | EMatch (rscrutinee, rbranches), EMatch (sscrutinee, sbranches) ->
        List.fold2_exn
          ~init:(recurse rscrutinee sscrutinee map)
          ~f:(fun map (_, (rparams, rrhs)) (_, (sparams, srhs)) ->
            recurse rrhs srhs (fst map, var_assert rparams sparams (snd map)))
          (List.sort rbranches ~compare:compare_branches)
          (List.sort sbranches ~compare:compare_branches)
    | ECtor (rhead, rargs), ECtor (shead, sargs) when String.equal rhead shead
      ->
        List.fold2_exn
          ~init:map
          ~f:(fun map rarg sarg -> recurse rarg sarg map)
          rargs
          sargs
    | EBase rbase, EBase sbase when [%eq: base_exp] rbase sbase -> map
    | ERScheme (rrs, rdt, rargs), ERScheme (srs, sdt, sargs)
      when [%eq: rscheme] rrs srs && String.equal rdt sdt ->
        List.fold2_exn
          ~init:map
          ~f:(fun map rarg sarg -> recurse rarg sarg map)
          rargs
          sargs
    | _, EHole (name, _) ->
        (match List.Assoc.find (fst map) name ~equal:String.equal with
        | Some binding ->
            assert ([%eq: exp] binding r);
            map
        | None -> ((name, r) :: fst map, snd map))
    | _, _ -> failwith "no match"
  in
  try Some (fst (recurse reference sketch ([], []))) with
  | _ -> None
