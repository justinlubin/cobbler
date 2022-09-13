open Core

(* Assumptions we have that are different from Huet's paper:
   - Atoms of distinct types have distinct variable names
   - Eta-expansion is allowed *)

(* === Typed lambda calculus used in Huet '74 === *)

(* Terms *)

type typ =
  | Elementary of string
  | Arrow of typ * typ
[@@deriving show, eq, sexp, ord]

type atom =
  | Variable of string * typ
  | Constant of string * typ
[@@deriving show, eq, sexp, ord]

type term =
  | Atom of atom
  | Application of term * term
  | Abstraction of string * typ * term
[@@deriving show, eq, sexp, ord]

let rec show_typ : typ -> string = function
  | Elementary x -> x
  | Arrow (t1, t2) -> sprintf "arr(%s, %s)" (show_typ t1) (show_typ t2)

let show_atom : atom -> string = function
  | Variable (x, _) -> sprintf "%s" x
  | Constant (c, _) -> sprintf "%s" c

let rec show_term : term -> string = function
  | Atom atom -> show_atom atom
  | Application (t1, t2) -> sprintf "(%s %s)" (show_term t1) (show_term t2)
  | Abstraction (param, _, body) -> sprintf "(lam %s %s)" param (show_term body)

(* Types *)

let atom_typ : atom -> typ = function
  | Variable (_, typ) -> typ
  | Constant (_, typ) -> typ

let rec typ : term -> typ = function
  | Atom atom -> atom_typ atom
  | Application (t1, t2) ->
      (match typ t1 with
      | Arrow (alpha, beta) ->
          if [%eq: typ] (typ t2) alpha
          then beta
          else failwith "argument type mismatch"
      | _ -> failwith "head not arrow type")
  | Abstraction (param, alpha, body) -> Arrow (alpha, typ body)

(* Substitution function *)

let suffix : int ref = ref (-1)

let gensym : unit -> string =
 fun () ->
  suffix := !suffix + 1;
  sprintf "__var%i" !suffix

let rec free_variables : term -> String.Set.t = function
  | Atom (Variable (x, _)) -> String.Set.singleton x
  | Atom (Constant (c, _)) -> String.Set.empty
  | Application (t1, t2) ->
      String.Set.union (free_variables t1) (free_variables t2)
  | Abstraction (param, _, body) ->
      String.Set.remove (free_variables body) param

let replace : string * string -> term -> term =
 fun (lhs, rhs) e ->
  let rec replace' = function
    | Atom (Variable (x, alpha)) ->
        if String.equal lhs x
        then Atom (Variable (rhs, alpha))
        else Atom (Variable (x, alpha))
    | Atom (Constant (c, alpha)) -> Atom (Constant (c, alpha))
    | Application (t1, t2) -> Application (replace' t1, replace' t2)
    | Abstraction (param, alpha, body) ->
        if String.equal lhs param
        then Abstraction (rhs, alpha, replace' body)
        else Abstraction (param, alpha, replace' body)
  in
  replace' e

let substitute : string * term -> term -> term =
 fun (lhs, rhs) e ->
  let rhs_fv = free_variables e in
  let rec substitute' = function
    | Atom (Variable (x, alpha)) ->
        if String.equal lhs x then rhs else Atom (Variable (x, alpha))
    | Atom (Constant (c, alpha)) -> Atom (Constant (c, alpha))
    | Application (t1, t2) -> Application (substitute' t1, substitute' t2)
    | Abstraction (param, alpha, body) ->
        if String.equal lhs param
        then Abstraction (param, alpha, body)
        else if not (String.Set.mem rhs_fv param)
        then Abstraction (param, alpha, substitute' body)
        else (
          let new_param = gensym () in
          Abstraction
            (new_param, alpha, substitute' (replace (param, new_param) body)))
  in
  substitute' e

(* Lambda conversion *)

let rec normal_order_step : term -> term option = function
  | Atom a -> None
  (* Outermost *)
  | Application (Abstraction (param, _, body), t2) ->
      Some (substitute (param, t2) body)
  | Application (t1, t2) ->
      (* Leftmost *)
      (match normal_order_step t1 with
      | Some t1' -> Some (Application (t1', t2))
      | None ->
          (match normal_order_step t2 with
          | Some t2' -> Some (Application (t1, t2'))
          | None -> None))
  | Abstraction (param, alpha, body) ->
      (match normal_order_step body with
      | Some body' -> Some (Abstraction (param, alpha, body'))
      | None -> None)

let rec normalize : term -> term =
 fun t ->
  match normal_order_step t with
  | Some t' -> normalize t'
  | None -> t

(* Normal form abbreviation *)

type abbreviation = (string * typ) list * atom * term list

let rec decompose_arr : typ -> typ list * typ = function
  | Elementary name -> ([], Elementary name)
  | Arrow (domain, codomain) ->
      let domain', codomain' = decompose_arr codomain in
      (domain :: domain', codomain')

let rec build_arr : typ list -> typ -> typ =
 fun domain codomain ->
  match domain with
  | [] -> codomain
  | hd :: tl -> Arrow (hd, build_arr tl codomain)

let strip_abstractions : term -> (string * typ) list * term =
 fun t ->
  let rec strip_abstractions' acc = function
    | Abstraction (param, alpha, body) ->
        strip_abstractions' ((param, alpha) :: acc) body
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

let build_abstractions : (string * typ) list -> term -> term =
 fun xs t ->
  List.fold_right xs ~init:t ~f:(fun (x, alpha) acc ->
      Abstraction (x, alpha, acc))

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

(* if n > 0 then (
      let vars = ws @ List.sub binding2 ~pos:n1 ~len:n in
      let h_domain = List.map ~f:snd vars in
      [ ( head1_var
        , build_abstractions
            vars
            (build_applications
               (Atom head2)
               (List.map argument2 ~f:(fun arg ->
                    let h = gensym () in
                    build_applications
                      (Atom (Variable (h, build_arr h_domain (typ arg))))
                      (List.map
                         ~f:(fun (x, typ) -> Atom (Variable (x, typ)))
                         vars)))) )
      ]) *)

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
