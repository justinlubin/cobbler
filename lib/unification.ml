open Core

(* SECTION 2: An overview of typed lambda-calculus *)

(* 2.2: Terms *)

type atom =
  | Variable of string
  | Constant of string

type term =
  | Atom of atom
  | Application of term * term
  | Abstraction of string * term

(* 2.3: Conversion *)

let suffix : int ref = ref (-1)

let gensym : unit -> string =
 fun () ->
  suffix := !suffix + 1;
  sprintf "__var%i" !suffix

let rec free_variables : term -> String.Set.t = function
  | Atom (Variable x) -> String.Set.singleton x
  | Atom (Constant c) -> String.Set.empty
  | Application (t1, t2) ->
      String.Set.union (free_variables t1) (free_variables t2)
  | Abstraction (param, body) -> String.Set.remove (free_variables body) param

let replace : string * string -> term -> term =
 fun (lhs, rhs) e ->
  let rec replace' = function
    | Atom (Variable x) ->
        if String.equal lhs x then Atom (Variable rhs) else Atom (Variable x)
    | Atom (Constant c) -> Atom (Constant c)
    | Application (t1, t2) -> Application (replace' t1, replace' t2)
    | Abstraction (param, body) ->
        if String.equal lhs param
        then Abstraction (rhs, replace' body)
        else Abstraction (param, replace' body)
  in
  replace' e

let substitute : string * term -> term -> term =
 fun (lhs, rhs) e ->
  let rhs_fv = free_variables e in
  let rec substitute' = function
    | Atom (Variable x) -> if String.equal lhs x then rhs else Atom (Variable x)
    | Atom (Constant c) -> Atom (Constant c)
    | Application (t1, t2) -> Application (substitute' t1, substitute' t2)
    | Abstraction (param, body) ->
        if String.equal lhs param
        then Abstraction (param, body)
        else if not (String.Set.mem rhs_fv param)
        then Abstraction (param, substitute' body)
        else (
          let new_param = gensym () in
          Abstraction (new_param, substitute' (replace (param, new_param) body)))
  in
  substitute' e

let rec normal_order_step : term -> term option = function
  | Atom a -> None
  (* Outermost *)
  | Application (Abstraction (param, body), t2) ->
      Some (substitute (param, t2) body)
  | Application (t1, t2) ->
      (* Leftmost *)
      (match normal_order_step t1 with
      | Some t1' -> Some (Application (t1', t2))
      | None ->
          (match normal_order_step t2 with
          | Some t2' -> Some (Application (t1, t2'))
          | None -> None))
  | Abstraction (param, body) ->
      (match normal_order_step body with
      | Some body' -> Some (Abstraction (param, body'))
      | None -> None)

let rec normalize : term -> term =
 fun t ->
  match normal_order_step t with
  | Some t' -> normalize t'
  | None -> t

type standard_form = SF of string list * atom * standard_form list

let strip_abstractions : term -> string list * term =
 fun t ->
  let rec strip_abstractions' acc = function
    | Abstraction (param, body) -> strip_abstractions' (param :: acc) body
    | rest -> (List.rev acc, rest)
  in
  strip_abstractions' [] t

let strip_applications : term -> term * term list =
 fun t ->
  let rec strip_applications' acc = function
    | Application (t1, t2) -> strip_applications' (t2 :: acc) t1
    | rest -> (rest, acc)
  in
  strip_applications' [] t

let rec standardize : term -> standard_form =
 fun t ->
  let binding, inside_term = strip_abstractions (normalize t) in
  let head_term, argument_terms = strip_applications inside_term in
  let head =
    match head_term with
    | Atom a -> a
    | _ -> failwith "non-normalized head"
  in
  SF (binding, head, List.map ~f:standardize argument_terms)

let heading : standard_form -> string list * atom = function
  | SF (binding, head, _) -> (binding, head)

let rigid : standard_form -> bool =
 fun sf ->
  let binding, head = heading sf in
  match head with
  | Variable x -> List.mem ~equal:String.equal binding x
  | Constant _ -> true

(* 2.4: Substitutions *)

type substitution_pair = string * term
type substitution = substitution_pair list

(* SECTION 3: The unification algorithm *)

type disagreement_set = (term * term) list

type matching_tree =
  | Terminal of bool
  | Nonterminal of disagreement_set * (substitution_pair * matching_tree) list

(* SIMPL procedure *)

let simpl : disagreement_set -> matching_tree = fun s -> failwith "TODO"

(* MATCH procedure *)

let matchh : term -> term -> String.Set.t -> substitution =
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
      assert (rigid (standardize e2));
      let sigma = matchh e1 e2 (failwith "free variables of N") in
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
                         (substitute sp e1', substitute sp e2'))
                       ds) ))
              sigma )
  | Nonterminal (ds, edges) ->
      Nonterminal (ds, List.map ~f:(fun (sp, mt) -> (sp, grow mt)) edges)

type unification_result =
  | Solved of (string * term) list
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

let unify : int -> term -> term -> unification_result =
 fun fuel e0 e0' -> search_and_grow fuel (simpl [ (e0, e0') ])
