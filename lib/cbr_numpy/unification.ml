open Lang
open Core
open Parse

let unique_hole_fail ~key:_ _ _ = failwith "Non-unique hole"

let merge_option_skewed
    : substitutions option -> substitutions option -> substitutions option
  =
 fun sub1 sub2 ->
  match (sub1, sub2) with
  | Some s1, Some s2 ->
      Some (String.Map.merge_skewed s1 s2 ~combine:unique_hole_fail)
  | _ -> None

let rec unify_expr : expr -> expr -> substitutions option =
 fun expr1 expr2 ->
  match expr2 with
  | Hole h ->
      (match expr1 with
      | Hole _ -> None
      | _ -> Some (String.Map.of_alist_exn [ (h, expr1) ]))
  | Num n2 ->
      (match expr1 with
      | Num n1 -> if n1 = n2 then Some String.Map.empty else None
      | _ -> None)
  | Index (index2, iter2) ->
      (match expr1 with
      | Index (index1, iter1) ->
          let sub1 = unify_expr index1 index2 in
          let sub2 = unify_expr iter1 iter2 in
          merge_option_skewed sub1 sub2
      | _ -> None)
  | Str s2 ->
      (match expr1 with
      | Str s1 ->
          if compare_string s1 s2 = 0 then Some String.Map.empty else None
      | _ -> None)
  | Call (name2, args2) ->
      (match expr1 with
      | Call (name1, args1) ->
          let args_list = List.zip args1 args2 in
          (match args_list with
          | List.Or_unequal_lengths.Unequal_lengths -> None
          | List.Or_unequal_lengths.Ok l ->
              let sub_names = unify_expr name1 name2 in
              let sub_args =
                List.fold
                  l
                  ~init:(Some String.Map.empty)
                  ~f:(fun sub_accum (arg1, arg2) ->
                    unify_expr arg1 arg2 |> merge_option_skewed sub_accum)
              in
              merge_option_skewed sub_names sub_args)
      | _ -> None)
  | Name name2 ->
      (match expr1 with
      | Name name1 ->
          if compare_string name1 name2 = 0 then Some String.Map.empty else None
      | _ -> None)

let rec unify_stmt : stmt -> stmt -> substitutions option =
 fun stmt1 stmt2 ->
  match stmt1 with
  | Assign (l1, r1) ->
      (match stmt2 with
      | Assign (l2, r2) ->
          if compare_lhs l1 l2 = 0 then unify_expr r1 r2 else None
      | For _ | Return _ -> None)
  | For (index1, iter1, body1) ->
      (match stmt2 with
      | For (index2, iter2, body2) ->
          if compare_id index1 index2 = 0
          then (
            let sub_iter = unify_expr iter1 iter2 in
            let sub_body = unify_block body1 body2 in
            merge_option_skewed sub_iter sub_body)
          else None
      | Assign _ | Return _ -> None)
  | Return expr1 ->
      (match stmt2 with
      | Return expr2 -> unify_expr expr1 expr2
      | _ -> None)

and unify_block : block -> block -> substitutions option =
 fun block1 block2 ->
  let block_list = List.zip block1 block2 in
  match block_list with
  | List.Or_unequal_lengths.Unequal_lengths -> None
  | List.Or_unequal_lengths.Ok l ->
      List.fold l ~init:(Some String.Map.empty) ~f:(fun sub (stmt1, stmt2) ->
          unify_stmt stmt1 stmt2 |> merge_option_skewed sub)

let unify_defn : defn -> defn -> substitutions option =
 fun (_, body1) (_, body2) -> unify_block body1 body2

let unify_env : env -> env -> substitutions option =
 fun env1 env2 ->
  if String.Map.length env1 <> String.Map.length env2
  then None
  else
    String.Map.fold
      env1
      ~init:(Some String.Map.empty)
      ~f:(fun ~key:name ~data:defn1 sub ->
        match String.Map.find env2 name with
        | None -> None
        | Some defn2 -> unify_defn defn1 defn2 |> merge_option_skewed sub)

let unify : program -> program -> substitutions option =
 fun (env1, block1) (env2, block2) ->
  match (unify_env env1 env2, unify_block block1 block2) with
  | Some sub1, Some sub2 ->
      Some
        (Map.merge sub1 sub2 ~f:(fun ~key:_ _ ->
             failwith "Non-unique hole name"))
  | None, _ -> None
  | _, None -> None
