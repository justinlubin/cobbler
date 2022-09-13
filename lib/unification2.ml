open Core
open Lang

(* Normal form abbreviation *)

type abbreviation = (string * typ) list * atom * term list

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
  let domain, codomain = decompose_arr (atom_typ head) in
  let eta_ws =
    List.map
      ~f:(fun alpha -> (gensym (), alpha))
      (List.drop domain (List.length argument_terms))
  in
  ( binding @ eta_ws
  , head
  , argument_terms
    @ List.map ~f:(fun (x, alpha) -> Atom (Variable (x, alpha))) eta_ws )

let rigid : term -> bool =
 fun t ->
  let binding, head, _ = abbreviate t in
  match head with
  | Variable (x, alpha) ->
      List.mem
        ~equal:(fun (x1, _) (x2, _) -> String.equal x1 x2)
        binding
        (x, alpha)
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

let headings_equal
    : (string * typ) list * atom -> (string * typ) list * atom -> bool
  =
 fun (binding1, head1) (binding2, head2) ->
  Int.equal (List.length binding1) (List.length binding2)
  && [%eq: term]
       (Atom head1)
       (normalize
          (build_applications
             (build_abstractions binding2 (Atom head2))
             (List.map
                ~f:(fun (x, alpha) -> Atom (Variable (x, alpha)))
                binding1)))

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

let matchh : term -> term -> substitution =
 fun e1 e2 ->
  let binding1, head1, argument1 = abbreviate e1 in
  let binding2, head2, argument2 = abbreviate e2 in
  let head1_var, head1_typ =
    match head1 with
    | Variable (x, alpha) -> (x, alpha)
    | _ -> failwith "head1 not flexible"
  in
  let n1 = List.length binding1 in
  let n2 = List.length binding2 in
  assert (Int.equal n1 n2);
  let p1 = List.length argument1 in
  (* let p2 = List.length argument2 in *)
  let domain, codomain = decompose_arr head1_typ in
  assert (Int.equal p1 (List.length domain));
  let should_imitate =
    match head2 with
    | Constant _ -> true
    | Variable (x, alpha) ->
        (match List.findi binding2 ~f:(fun _ (v, _) -> String.equal x v) with
        | None -> false
        | Some (i, _) -> i >= n1)
  in
  let ws = List.map ~f:(fun alpha -> (gensym (), alpha)) domain in
  let imitation =
    if not should_imitate
    then []
    else
      [ ( head1_var
        , build_abstractions
            ws
            (build_applications
               (Atom head2)
               (List.map argument2 ~f:(fun arg ->
                    let h = gensym () in
                    build_applications
                      (Atom (Variable (h, build_arr domain (typ arg))))
                      (List.map
                         ~f:(fun (w, typ) -> Atom (Variable (w, typ)))
                         ws)))) )
      ]
  in
  let projection =
    List.filter_map ws ~f:(fun (w, w_typ) ->
        let w_domain, w_codomain = decompose_arr w_typ in
        if [%eq: typ] w_codomain codomain
        then
          Some
            ( head1_var
            , build_abstractions
                ws
                (build_applications
                   (Atom (Variable (w, w_typ)))
                   (List.map w_domain ~f:(fun w_arg_typ ->
                        let h = gensym () in
                        build_applications
                          (Atom (Variable (h, build_arr domain w_arg_typ)))
                          (List.map
                             ~f:(fun (w, typ) -> Atom (Variable (w, typ)))
                             ws)))) )
        else None)
  in
  imitation @ projection

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
      let sigma = matchh e1 e2 in
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
                         ( normalize (substitute sp e1')
                         , normalize (substitute sp e2') ))
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

let unify' : int -> disagreement_set -> unification_result =
 fun fuel ds -> search_and_grow fuel (simpl ds)

(* === Examples === *)

let ds1 =
  [ ( Application
        ( Application
            ( Atom
                (Constant
                   ( "A"
                   , Arrow
                       ( Arrow (Elementary "Bin2", Elementary "Bout")
                       , Arrow (Elementary "Cout", Elementary "Aout") ) ))
            , Abstraction
                ( "u"
                , Elementary "Bin2"
                , Application
                    ( Application
                        ( Atom
                            (Constant
                               ( "B"
                               , Arrow
                                   ( Elementary "Bin1"
                                   , Arrow (Elementary "Bin2", Elementary "Bout")
                                   ) ))
                        , Atom (Variable ("x", Elementary "Bin1")) )
                    , Atom (Variable ("u", Elementary "Bin2")) ) ) )
        , Atom (Constant ("C", Elementary "Cout")) )
    , Application
        ( Application
            ( Atom
                (Constant
                   ( "A"
                   , Arrow
                       ( Arrow (Elementary "Bin2", Elementary "Bout")
                       , Arrow (Elementary "Cout", Elementary "Aout") ) ))
            , Abstraction
                ( "v"
                , Elementary "Bin2"
                , Application
                    ( Application
                        ( Atom
                            (Constant
                               ( "B"
                               , Arrow
                                   ( Elementary "Bin1"
                                   , Arrow (Elementary "Bin2", Elementary "Bout")
                                   ) ))
                        , Atom (Variable ("y", Elementary "Bin1")) )
                    , Atom (Variable ("v", Elementary "Bin2")) ) ) )
        , Application
            ( Atom (Variable ("f", Arrow (Elementary "Cout", Elementary "Cout")))
            , Atom (Constant ("C", Elementary "Cout")) ) ) )
  ]

(*
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
*)
let _ =
  [%test_result: matching_tree]
    (simpl ds1)
    ~expect:
      (Nonterminal
         ( [ ( Abstraction
                 ( "u"
                 , Elementary "Bin2"
                 , Atom (Variable ("x", Elementary "Bin1")) )
             , Abstraction
                 ( "v"
                 , Elementary "Bin2"
                 , Atom (Variable ("y", Elementary "Bin1")) ) )
           ; ( Application
                 ( Atom
                     (Variable
                        ("f", Arrow (Elementary "Cout", Elementary "Cout")))
                 , Atom (Constant ("C", Elementary "Cout")) )
             , Atom (Constant ("C", Elementary "Cout")) )
           ]
         , [] ))

(*
let _ = [%test_result: matching_tree] (simpl ds2) ~expect:(Terminal true)
let _ = [%test_result: matching_tree] (simpl ds3) ~expect:(Terminal false)
*)

let ds4 =
  let f =
    Atom (Variable ("f", Arrow (Elementary "gamma", Elementary "gamma")))
  in
  let x = Atom (Variable ("x", Elementary "gamma")) in
  let a =
    Atom (Constant ("A", Arrow (Elementary "gamma", Elementary "gamma")))
  in
  let b = Atom (Constant ("B", Elementary "gamma")) in
  [ (Application (f, Application (f, x)), Application (a, Application (a, b))) ]

let ds5 =
  let x = Atom (Variable ("x", Elementary "gamma")) in
  let y =
    Atom (Variable ("y", Arrow (Elementary "gamma", Elementary "gamma")))
  in
  let c =
    Atom (Constant ("C", Arrow (Elementary "gamma", Elementary "gamma")))
  in
  [ ( Abstraction
        ( "x"
        , Elementary "gamma"
        , Application (y, Application (c, Application (y, x))) )
    , Abstraction ("x", Elementary "gamma", Application (c, x)) )
  ]
