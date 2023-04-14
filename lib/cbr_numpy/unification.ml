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

let rec unify_expr : substitutions option -> expr -> expr -> substitutions option =
 fun subs_opt expr1 expr2 ->
  match subs_opt with
  | None -> None
  | Some subs ->
  match expr2 with
  | Hole (_, h) when String.Map.mem subs h ->
      if equal_expr expr1 (String.Map.find_exn subs h)
      then Some subs
      else None
  | Hole (_, h) ->
      Some (String.Map.add_exn subs ~key:h ~data:expr1)
  | Num n2 ->
      (match expr1 with
      | Num n1 when Int.equal n1 n2 -> Some subs
      | _ -> None)
  | Index (index2, iter2) ->
      (match expr1 with
      | Index (index1, iter1) ->
          ((subs_opt |> unify_expr) iter1 iter2 |>  unify_expr) index1 index2
      | _ -> None)
  | Str s2 ->
      (match expr1 with
      | Str s1 when String.equal s1 s2 -> Some subs
      | _ -> None)
  | Call (name2, args2) ->
      (match expr1 with
      | Call (name1, args1) ->
          let args_list = List.zip args1 args2 in
          (match args_list with
          | List.Or_unequal_lengths.Unequal_lengths -> None
          | List.Or_unequal_lengths.Ok l ->
              let sub_args = List.fold2_exn args1 args2 ~init:subs_opt ~f:unify_expr in
              let sub_names = unify_expr sub_args name1 name2 in
              sub_names )
      | _ -> None)
  | Name name2 ->
      (match expr1 with
      | Name name1 when String.equal name1 name2 -> Some subs
      | _ -> None)

let rec unify_pat : substitutions option -> pat -> pat -> substitutions option =
 fun subs_opt pat1 pat2 ->
  match subs_opt with 
  | None -> None
  | Some subs ->
  match pat2 with 
  | PName _ when equal_pat pat1 pat2 -> let _ = print_string "checkpoint" in Some subs 
  | PName _ -> None
  | PHole (_, h) when String.Map.mem subs h -> Some subs
  | PHole (_, h) ->
    ( match pat1 with 
      | PName n -> Some (String.Map.add_exn subs ~key:h ~data:(Name n))
      (* maybe need to consider case of matching hole with index*)
      | _ -> None
    )
  | PIndex (l2, r2) ->
    ( match pat1 with 
      | PIndex (l1, r1) -> (unify_pat subs_opt l1 l2 |> unify_expr) r1 r2
      | _ -> None 
    )

let rec unify_stmt : substitutions option -> stmt -> stmt -> substitutions option =
 fun subs_opt stmt1 stmt2 ->
  match subs_opt with 
  | None -> None
  | Some subs ->
  match stmt1 with
  | Assign (l1, r1) ->
      (match stmt2 with
      | Assign (l2, r2) -> (unify_expr subs_opt r1 r2 |> unify_pat) l1 l2
      | _ -> None)
  | For (index1, iter1, body1) ->
      (match stmt2 with
      | For (index2, iter2, body2) ->
          let sub_iter = unify_expr subs_opt iter1 iter2 in
          let sub_index = unify_pat sub_iter index1 index2 in
          unify_block sub_index body1 body2
      | _ -> None)
  | Return expr1 ->
      (match stmt2 with
      | Return expr2 -> unify_expr subs_opt expr1 expr2
      | _ -> None)

and unify_block : substitutions option -> block -> block -> substitutions option =
 fun subs_opt block1 block2 ->
  let block_list = List.zip block1 block2 in
  match block_list with
  | List.Or_unequal_lengths.Unequal_lengths -> None
  | List.Or_unequal_lengths.Ok l ->
      List.fold l ~init:subs_opt ~f:(fun subs (stmt1, stmt2) ->
          unify_stmt subs stmt1 stmt2)

let unify_defn : defn -> defn -> substitutions option =
 fun (_, body1) (_, body2) -> unify_block (Some String.Map.empty) body1 body2

let unify_env : env -> env -> substitutions option =
 fun env1 env2 ->
  if not (Int.equal (String.Map.length env1) (String.Map.length env2))
  then None
  else
    String.Map.fold
      env1
      ~init:(Some String.Map.empty)
      ~f:(fun ~key:name ~data:defn1 sub ->
        match String.Map.find env2 name with
        | None -> None
        | Some defn2 -> unify_defn defn1 defn2 |> merge_option_skewed sub)

let unify : target:program -> pattern:program -> substitutions option =
 fun ~target:(env1, block1) ~pattern:(env2, block2) ->
  unify_block (Some String.Map.empty) block1 block2
