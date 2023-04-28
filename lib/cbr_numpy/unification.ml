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

let rec unify_expr
    : substitutions option -> expr -> expr -> substitutions option
  =
 fun subs_opt expr1 expr2 ->
  match subs_opt with
  | None -> None
  | Some subs ->
      (match expr2 with
      | Hole (_, h) when String.Map.mem subs h ->
          if equal_expr expr1 (String.Map.find_exn subs h)
          then Some subs
          else None
      | Hole (_, h) -> Some (String.Map.add_exn subs ~key:h ~data:expr1)
      | Num n2 ->
          (match expr1 with
          | Num n1 when Int.equal n1 n2 -> Some subs
          | _ -> None)
      | Index (index2, iter2) ->
          (match expr1 with
          | Index (index1, iter1) ->
              ((subs_opt |> unify_expr) iter1 iter2 |> unify_expr) index1 index2
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
                  let sub_args =
                    List.fold2_exn args1 args2 ~init:subs_opt ~f:unify_expr
                  in
                  let sub_names = unify_expr sub_args name1 name2 in
                  sub_names)
          | _ -> None)
      | Name name2 ->
          (match expr1 with
          | Name name1 when String.equal name1 name2 -> Some subs
          | _ -> None))

let rec unify_pat : substitutions option -> pat -> pat -> substitutions option =
 fun subs_opt pat1 pat2 ->
  match subs_opt with
  | None -> None
  | Some subs ->
      (match pat2 with
      | PName _ when equal_pat pat1 pat2 ->
          let _ = print_string "checkpoint" in
          Some subs
      | PName _ -> None
      | PHole (_, h) when String.Map.mem subs h -> Some subs
      | PHole (_, h) ->
          (match pat1 with
          | PName n -> Some (String.Map.add_exn subs ~key:h ~data:(Name n))
          (* maybe need to consider case of matching hole with index*)
          | _ -> None)
      | PIndex (l2, r2) ->
          (match pat1 with
          | PIndex (l1, r1) -> (unify_pat subs_opt l1 l2 |> unify_expr) r1 r2
          | _ -> None))

let rec unify_stmt
    : substitutions option -> stmt -> stmt -> substitutions option
  =
 fun subs_opt stmt1 stmt2 ->
  match subs_opt with
  | None -> None
  | Some subs ->
      (match stmt1 with
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
          | _ -> None))

and unify_block : substitutions option -> block -> block -> substitutions option
  =
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

let unify_naive
    :  ?debug:bool -> target:program -> pattern:program -> unit
    -> substitutions option
  =
 fun ?(debug = false) ~target:(env1, block1) ~pattern:(env2, block2) () ->
  unify_block (Some String.Map.empty) block1 block2

let commutative_add = ("(Call Name_+ ?a ?b)", "(Call Name_+ ?b ?a)")
let commutative_mul = ("(Call Name_* ?a ?b)", "(Call Name_* ?b ?a)")
let identity_add = ("(Call Name_+ ?a Num_0)", "?a")
let identity_mul = ("(Call Name_* ?a Num_1)", "?a")

let rewrite_rules =
  [ commutative_add; commutative_mul; identity_add; identity_mul ]
  |> List.map ~f:(fun (from, into) ->
         EGraph.Rule.make_constant
           ~from:(Sexp.of_string from |> Query.of_sexp op_of_string)
           ~into:(Sexp.of_string into |> Query.of_sexp op_of_string))

type hole_map = string String.Map.t

let query_of_prog : program -> hole_map * 'a Query.t =
 fun p ->
  let rec replace_holes_pat : hole_map -> pat -> hole_map * Sexp.t =
   fun map l ->
    match l with
    | PIndex (pat, index) ->
        let map, pat = replace_holes_pat map pat in
        let map, index = replace_holes_expr map index in
        (map, Sexp.List [ Sexp.Atom "Index"; pat; index ])
    | PName n -> (map, Sexp.Atom ("Name_" ^ n))
    | PHole (_, h) ->
        if String.Map.mem map h
        then (map, Sexp.Atom (String.Map.find_exn map h))
        else (
          let new_name = gensym "?" |> String.chop_prefix_exn ~prefix:"__" in
          (String.Map.add_exn map ~key:h ~data:new_name, Sexp.Atom new_name))
  and replace_holes_expr : hole_map -> expr -> hole_map * Sexp.t =
   fun map e ->
    match e with
    | Hole (_, h) ->
        if String.Map.mem map h
        then (map, Sexp.Atom (String.Map.find_exn map h))
        else (
          let new_name = gensym "?" |> String.chop_prefix_exn ~prefix:"__" in
          (String.Map.add_exn map ~key:h ~data:new_name, Sexp.Atom new_name))
    | Name n -> (map, Sexp.Atom ("Name_" ^ n))
    | Num n -> (map, Sexp.Atom ("Num_" ^ string_of_int n))
    | Index (pat, index) ->
        let map, pat = replace_holes_expr map pat in
        let map, index = replace_holes_expr map index in
        (map, Sexp.List [ Sexp.Atom "Index"; pat; index ])
    | Call (func, args) ->
        let map, func = replace_holes_expr map func in
        let map, args = List.fold_map args ~init:map ~f:replace_holes_expr in
        (map, Sexp.List ([ Sexp.Atom "Call"; func ] @ args))
    | Str s -> (map, Sexp.Atom ("Str_" ^ s))
  in
  let rec replace_holes_stmt : hole_map -> stmt -> hole_map * Sexp.t =
   fun map s ->
    match s with
    | Return e ->
        let map, e = replace_holes_expr map e in
        (map, Sexp.List [ Sexp.Atom "Return"; e ])
    | Assign (lhs, rhs) ->
        let map, lhs = replace_holes_pat map lhs in
        let map, rhs = replace_holes_expr map rhs in
        (map, Sexp.List [ Sexp.Atom "Assign"; lhs; rhs ])
    | For (index, iter, body) ->
        let map, index = replace_holes_pat map index in
        let map, iter = replace_holes_expr map iter in
        let map, body = replace_holes_block map body in
        (map, Sexp.List [ Sexp.Atom "For"; index; iter; body ])
  and replace_holes_block : hole_map -> block -> hole_map * Sexp.t =
   fun map b ->
    let map, l = List.fold_map b ~init:map ~f:replace_holes_stmt in
    (map, Sexp.List ([ Sexp.Atom "Block" ] @ l))
  in
  let replace_holes_prog : program -> hole_map * Sexp.t =
   fun (e, b) ->
    let map = String.Map.empty in
    let map, b = replace_holes_block map b in
    (map, Sexp.List [ Sexp.Atom "Prog"; b ])
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

let unify_egraph
    :  ?debug:bool -> target:program -> pattern:program -> unit
    -> substitutions option
  =
 fun ?(debug = false) ~target ~pattern () ->
  let graph = EGraph.init () in
  let t = sexp_of_program target |> t_of_sexp in
  let _ = EGraph.add_node graph t in
  if debug
  then (
    print_endline ("\nTarget: \n" ^ (sexp_of_t t |> Sexp.to_string));
    EGraph.to_dot graph |> Odot.print_file "before_eqsat.txt")
  else ();
  let _ = EGraph.run_until_saturation graph rewrite_rules in
  if debug then EGraph.to_dot graph |> Odot.print_file "after_eqsat.txt" else ();
  let map, q = query_of_prog pattern in
  if debug
  then
    print_endline
      ("Pattern:\n" ^ (Query.to_sexp string_of_op q |> Sexp.to_string))
  else ();
  let matches = EGraph.find_matches (EGraph.freeze graph) q in
  extract_matches graph matches map
