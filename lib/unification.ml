open Core
open Lang

(* Nomenclature used in Huet's article *)

type substitution_pair = id * exp
type substitution = substitution_pair list
type disagreement_set = (exp * exp) list

type matching_tree =
  | Terminal of bool
  | Nonterminal of disagreement_set * (substitution_pair * matching_tree) list

(* Helpers *)

let rigid : exp -> bool = fun e -> failwith "TODO"

(* SIMPL procedure *)

let simpl : disagreement_set -> matching_tree = fun s -> failwith "TODO"

(* MATCH procedure *)

let matchh : exp -> exp -> (id, String.comparator_witness) Set.t -> substitution
  =
 fun s -> failwith "TODO"

(* Top-level algorithm *)

let rec search : matching_tree -> substitution option = function
  | Terminal true -> Some []
  | Terminal false -> None
  | Nonterminal (_, edges) ->
      List.find_map
        ~f:(fun (sp, child) ->
          Option.map ~f:(fun sigma -> sp :: sigma) (search child))
        edges

let rec saturated : matching_tree -> bool = function
  | Terminal _ -> true
  | Nonterminal (_, []) -> false
  | Nonterminal (_, edges) ->
      List.exists ~f:(fun (_, child) -> saturated child) edges

let rec grow : matching_tree -> matching_tree = function
  | Terminal sf -> Terminal sf
  | Nonterminal ([], _) ->
      failwith "non-reduced (empty) disagreement set in nonterminal"
  | Nonterminal (((e1, e2) :: tl as ds), []) ->
      (* Choosing the first pair is arbitrary *)
      assert (rigid e2);
      let sigma = matchh e1 e2 (failwith "TODO") in
      if List.is_empty sigma
      then Terminal false
      else
        Nonterminal
          ( ds
          , List.map
              ~f:(fun sp ->
                ( sp
                , simpl
                    (List.map
                       ~f:(fun (e1', e2') ->
                         ( Lang_util.substitute sp e1'
                         , Lang_util.substitute sp e2' ))
                       ds) ))
              sigma )
  | Nonterminal (ds, edges) ->
      Nonterminal (ds, List.map ~f:(fun (sp, mt) -> (sp, grow mt)) edges)

type unification_result =
  | Solved of (id * exp) list
  | Impossible
  | OutOfFuel

let rec search_and_grow : int -> matching_tree -> unification_result =
 fun fuel m ->
  if fuel <= 0
  then OutOfFuel
  else (
    match search m with
    | Some sigma -> Solved sigma
    | None ->
        if saturated m then Impossible else search_and_grow (fuel - 1) (grow m))

let unify : int -> exp -> exp -> unification_result =
 fun fuel e0 e0' -> search_and_grow fuel (simpl [ (e0, e0') ])
