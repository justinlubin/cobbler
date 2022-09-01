open Core

(* === Typed lambda calculus used in Huet '74 === *)

(* Terms *)

type atom =
  | Variable of string
  | Constant of string
[@@deriving show, eq, sexp, ord]

type term =
  | Atom of atom
  | Application of term * term
  | Abstraction of string * term
[@@deriving show, eq, sexp, ord]

(* Substitution function *)

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

(* Lambda conversion *)

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

(* Normal form abbreviation *)

type abbreviation = string list * atom * term list

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

let build_abstractions : string list -> term -> term =
 fun xs t -> List.fold_right xs ~init:t ~f:(fun x acc -> Abstraction (x, acc))

let build_applications : term -> term list -> term =
 fun head args ->
  List.fold_left args ~init:head ~f:(fun acc arg -> Application (acc, arg))

(* Assumes input is normalized *)
let abbreviate : term -> abbreviation =
 fun t ->
  let binding, inside_term = strip_abstractions t in
  let head_term, argument_terms = strip_applications inside_term in
  let head =
    match head_term with
    | Atom a -> a
    | _ -> failwith "non-normalized head"
  in
  (binding, head, argument_terms)

let rigid : term -> bool =
 fun t ->
  let binding, head, _ = abbreviate t in
  match head with
  | Variable x -> List.mem ~equal:String.equal binding x
  | Constant _ -> true

(* Substitutions *)

type substitution_pair = string * term [@@deriving show, eq, sexp, ord]
type substitution = substitution_pair list [@@deriving show, eq, sexp, ord]

(* === The unification algorithm ===  *)

(* Types *)

type disagreement_set = (term * term) list [@@deriving show, eq, sexp, ord]

type matching_tree =
  | Terminal of bool
  | Nonterminal of disagreement_set * (substitution_pair * matching_tree) list
[@@deriving show, eq, sexp, ord]

(* SIMPL procedure *)

let headings_equal : string list * atom -> string list * atom -> bool =
 fun (binding1, head1) (binding2, head2) ->
  Int.equal (List.length binding1) (List.length binding2)
  && [%eq: term]
       (Atom head1)
       (normalize
          (build_applications
             (build_abstractions binding2 (Atom head2))
             (List.map ~f:(fun x -> Atom (Variable x)) binding1)))

let rec simpl_step1 : disagreement_set -> disagreement_set option =
 fun ds ->
  match
    Util.find_and_remove_first ds ~f:(fun (e1, e2) -> rigid e1 && rigid e2)
  with
  | Some ((e1, e2), rest_ds) ->
      let binding1, head1, argument1 = abbreviate e1 in
      let binding2, head2, argument2 = abbreviate e2 in
      if headings_equal (binding1, head1) (binding2, head2)
      then (
        let new_ds =
          List.map2_exn argument1 argument2 ~f:(fun e1 e2 ->
              (build_abstractions binding1 e1, build_abstractions binding2 e2))
        in
        simpl_step1 (new_ds @ rest_ds))
      else None
  | None -> Some ds

let simpl_step2 : disagreement_set -> disagreement_set =
 fun ds ->
  List.map
    ~f:(fun (e1, e2) ->
      if rigid e1 && not (rigid e2) then (e2, e1) else (e1, e2))
    ds

let simpl_step3 : disagreement_set -> matching_tree =
 fun ds ->
  if List.exists ~f:(fun (e1, e2) -> rigid e2) ds
  then Nonterminal (ds, [])
  else Terminal true

let simpl : disagreement_set -> matching_tree =
 fun ds ->
  match simpl_step1 ds with
  | Some ds -> simpl_step3 (simpl_step2 ds)
  | None -> Terminal false

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
      List.for_all ~f:(fun (_, child) -> saturated child) edges

let rec grow : matching_tree -> matching_tree = function
  | Terminal sf -> Terminal sf
  | Nonterminal ([], _) ->
      failwith "non-reduced (empty) disagreement set in nonterminal"
  | Nonterminal (((e1, e2) :: tl as ds), []) ->
      (* Choosing the first pair is arbitrary *)
      assert (rigid e2);
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

(* === Examples === *)

let ds1 =
  [ ( Application
        ( Application
            ( Atom (Constant "A")
            , Abstraction
                ( "u"
                , Application
                    ( Application (Atom (Constant "B"), Atom (Variable "x"))
                    , Atom (Variable "u") ) ) )
        , Atom (Constant "C") )
    , Application
        ( Application
            ( Atom (Constant "A")
            , Abstraction
                ( "v"
                , Application
                    ( Application (Atom (Constant "B"), Atom (Variable "y"))
                    , Atom (Variable "v") ) ) )
        , Application (Atom (Variable "f"), Atom (Constant "C")) ) )
  ]

let ds2 =
  [ ( Application
        ( Atom (Constant "A")
        , Abstraction
            ( "u"
            , Application
                ( Application (Atom (Constant "B"), Atom (Variable "x"))
                , Atom (Variable "u") ) ) )
    , Application
        ( Atom (Constant "A")
        , Abstraction
            ( "v"
            , Application
                ( Application (Atom (Constant "B"), Atom (Variable "y"))
                , Atom (Variable "v") ) ) ) )
  ]

let ds3 =
  [ ( Abstraction
        ( "u"
        , Abstraction
            ( "v"
            , Application
                ( Application (Atom (Constant "A"), Atom (Variable "u"))
                , Abstraction ("w", Atom (Variable "v")) ) ) )
    , Abstraction
        ( "v"
        , Abstraction
            ( "w"
            , Application
                ( Application (Atom (Constant "A"), Atom (Variable "v"))
                , Abstraction ("u", Atom (Variable "v")) ) ) ) )
  ]

let _ =
  [%test_result: matching_tree]
    (simpl ds1)
    ~expect:
      (Nonterminal
         ( [ ( Abstraction ("u", Atom (Variable "x"))
             , Abstraction ("v", Atom (Variable "y")) )
           ; ( Application (Atom (Variable "f"), Atom (Constant "C"))
             , Atom (Constant "C") )
           ]
         , [] ))

let _ = [%test_result: matching_tree] (simpl ds2) ~expect:(Terminal true)
let _ = [%test_result: matching_tree] (simpl ds3) ~expect:(Terminal false)
