open Lang
open Core
open Parse
open Ego.Generic
open Egraph
open Util

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
let identity_mul = ("(Call EName* ?a Num1)", "?a")

let rewrite_rules =
  [ commutative_add; commutative_mul; identity_add; identity_mul ]
  |> List.map ~f:(fun (from, into) ->
         EGraph.Rule.make_constant
           ~from:(Sexp.of_string from |> Query.of_sexp op_of_string)
           ~into:(Sexp.of_string into |> Query.of_sexp op_of_string))

type hole_map = string String.Map.t

let query_of_prog : program -> hole_map * 'a Query.t =
 fun p ->
  let replace_holes_id : hole_map -> id -> hole_map * Sexp.t =
   fun map id -> (map, Sexp.Atom id)
  in
  let rec replace_holes_lhs : hole_map -> lhs -> hole_map * Sexp.t =
   fun map l ->
    match l with
    | Index (lhs, index) ->
        let map, lhs = replace_holes_lhs map lhs in
        let map, index = replace_holes_expr map index in
        (map, Sexp.List [ Sexp.Atom "LIndex"; lhs; index ])
    | Name n -> (map, Sexp.Atom ("LName" ^ n))
  and replace_holes_expr : hole_map -> expr -> hole_map * Sexp.t =
   fun map e ->
    match e with
    | Hole h ->
        if String.Map.mem map h
        then (map, Sexp.Atom (String.Map.find_exn map h))
        else (
          let new_name = gensym "?" |> String.chop_prefix_exn ~prefix:"__" in
          (String.Map.add_exn map ~key:h ~data:new_name, Sexp.Atom new_name))
    | Name n -> (map, Sexp.Atom ("EName" ^ n))
    | Num n -> (map, Sexp.Atom ("Num" ^ string_of_int n))
    | Index (lhs, index) ->
        let map, lhs = replace_holes_expr map lhs in
        let map, index = replace_holes_expr map index in
        (map, Sexp.List [ Sexp.Atom "ExprIndex"; lhs; index ])
    | Call (func, args) ->
        let map, func = replace_holes_expr map func in
        let map, args = List.fold_map args ~init:map ~f:replace_holes_expr in
        (map, Sexp.List ([ Sexp.Atom "Call"; func ] @ args))
    | Str s -> (map, Sexp.Atom ("Str" ^ s))
  in
  let rec replace_holes_stmt : hole_map -> stmt -> hole_map * Sexp.t =
   fun map s ->
    match s with
    | Return e ->
        let map, e = replace_holes_expr map e in
        (map, Sexp.List [ Sexp.Atom "Return"; e ])
    | Assign (lhs, rhs) ->
        let map, lhs = replace_holes_lhs map lhs in
        let map, rhs = replace_holes_expr map rhs in
        (map, Sexp.List [ Sexp.Atom "Assign"; lhs; rhs ])
    | For (index, iter, body) ->
        let map, index = replace_holes_id map index in
        let map, iter = replace_holes_expr map iter in
        let map, body = replace_holes_block map body in
        (map, Sexp.List [ Sexp.Atom "For"; index; iter; body ])
  and replace_holes_block : hole_map -> block -> hole_map * Sexp.t =
   fun map b ->
    let map, l = List.fold_map b ~init:map ~f:replace_holes_stmt in
    (map, Sexp.List ([ Sexp.Atom "Block" ] @ l))
  in
  let replace_holes_defn : hole_map -> defn -> hole_map * Sexp.t =
   fun map (args, body) ->
    let map, args = List.fold_map args ~init:map ~f:replace_holes_id in
    let map, body = replace_holes_block map body in
    (map, Sexp.List [ Sexp.Atom "Defn"; Sexp.List args; body ])
  in
  let replace_holes_env : hole_map -> env -> hole_map * Sexp.t =
   fun map e ->
    let map, l =
      String.Map.to_alist e
      |> List.fold_map ~init:map ~f:(fun map (name, d) ->
             let map, d = replace_holes_defn map d in
             (map, Sexp.List [ Sexp.Atom name; d ]))
    in
    (map, Sexp.List ([ Sexp.Atom "Env" ] @ l))
  in
  let replace_holes_prog : program -> hole_map * Sexp.t =
   fun (e, b) ->
    let map = String.Map.empty in
    let map, e = replace_holes_env map e in
    let map, b = replace_holes_block map b in
    (map, Sexp.List [ Sexp.Atom "Prog"; e; b ])
  in
  let map, p = replace_holes_prog p in
  (map, Query.of_sexp op_of_string p)

let extract_matches
    :  rw EGraph.t -> (Ego.Id.t * Ego.Id.t StringMap.t) Iter.t -> hole_map
    -> substitutions option
  =
 fun graph matches hole_names ->
  let map =
    String.Map.to_alist hole_names
    |> List.fold ~init:String.Map.empty ~f:(fun map (prog_hole, egraph_hole) ->
           let egraph_hole = String.chop_prefix_exn egraph_hole ~prefix:"?" in
           match
             Iter.find_pred
               (fun (_, match_map) -> StringMap.mem egraph_hole match_map)
               matches
           with
           | None -> map
           | Some (_, match_map) ->
               let id = StringMap.find egraph_hole match_map in
               String.Map.add_exn
                 map
                 ~key:prog_hole
                 ~data:(Extractor.extract graph id |> sexp_of_t |> expr_of_sexp))
  in
  match String.Map.to_alist map with
  | [] -> None
  | _ -> Some map

let unify_egraph : program -> program -> substitutions option =
 fun p1 p2 ->
  let graph = EGraph.init () in
  let _ = sexp_of_program p1 |> t_of_sexp |> EGraph.add_node graph in
  EGraph.to_dot graph |> Odot.print_file "before_eqsat.txt";
  let _ = EGraph.run_until_saturation graph rewrite_rules in
  EGraph.to_dot graph |> Odot.print_file "after_eqsat.txt";
  let map, q = query_of_prog p2 in
  let matches = EGraph.find_matches (EGraph.freeze graph) q in
  extract_matches graph matches map
