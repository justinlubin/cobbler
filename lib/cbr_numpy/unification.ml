open Lang
open Core
open Parse
open Ego.Generic
open Egraph

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
      | _ -> Some (String.Map.of_alist_exn [ (h, expr1) ]))
  | Num n2 ->
      (match expr1 with
      | Num n1 when Int.equal n1 n2 -> Some String.Map.empty
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
      | Str s1 when String.equal s1 s2 -> Some String.Map.empty
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
      | Name name1 when String.equal name1 name2 -> Some String.Map.empty
      | _ -> None)

let rec unify_stmt : stmt -> stmt -> substitutions option =
 fun stmt1 stmt2 ->
  match stmt1 with
  | Assign (l1, r1) ->
      (match stmt2 with
      | Assign (l2, r2) when equal_lhs l1 l2 -> unify_expr r1 r2
      | _ -> None)
  | For (index1, iter1, body1) ->
      (match stmt2 with
      | For (index2, iter2, body2) when equal_id index1 index2 ->
          let sub_iter = unify_expr iter1 iter2 in
          let sub_body = unify_block body1 body2 in
          merge_option_skewed sub_iter sub_body
      | _ -> None)
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

let unify_naive : program -> program -> substitutions option =
 fun (env1, block1) (env2, block2) ->
  merge_option_skewed (unify_env env1 env2) (unify_block block1 block2)

let commutative_add = ("(Call EName+ ?a ?b)", "(Call EName+ ?b ?a)")
let commutative_mul = ("(Call EName* ?a ?b)", "(Call EName* ?b ?a)")
let identity_add = ("(Call EName+ ?a Num0)", "?a")
let identity_mul = ("(Call EName* ?a Num1", "?a")

let rewrite_rules =
  [ commutative_add; commutative_mul; identity_add; identity_mul ]
  |> List.map ~f:(fun (from, into) ->
         EGraph.Rule.make_constant
           ~from:(Sexp.of_string from |> Query.of_sexp op_of_string)
           ~into:(Sexp.of_string into |> Query.of_sexp op_of_string))

let unify_egraph : program -> program -> substitutions option =
 fun p1 p2 ->
  let graph = EGraph.init () in
  let expr_id1 = sexp_of_program p1 |> t_of_sexp |> EGraph.add_node graph in
  let expr_id2 = sexp_of_program p2 |> t_of_sexp |> EGraph.add_node graph in
  EGraph.to_dot graph |> Odot.print_file "before_eqsat.txt";
  let eq_before = EGraph.class_equal (EGraph.freeze graph) expr_id1 expr_id2 in
  if eq_before then printf "Equal\n" else printf "Not equal\n";
  let _ = EGraph.run_until_saturation graph rewrite_rules in
  let eq_after = EGraph.class_equal (EGraph.freeze graph) expr_id1 expr_id2 in
  if eq_after then printf "Success" else printf "Failure";
  EGraph.to_dot graph |> Odot.print_file "after_eqsat.txt";
  None
