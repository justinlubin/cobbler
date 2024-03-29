open Core

(* === Typed lambda calculus used in Huet '74 === *)

(* Terms *)

type typ =
  | Elementary of Lang.typ
  | Arrow of typ * typ
[@@deriving sexp, ord, show]

type atom =
  | Variable of string * typ
  | Constant of string * typ
[@@deriving sexp, ord, show]

type term =
  | Atom of atom
  | Application of term * term
  | Abstraction of string * typ * term
[@@deriving sexp, ord, show]

let rec show_typ : typ -> string =
 fun tau ->
  match tau with
  | Elementary tau' -> sprintf "<%s>" (Typ.show tau')
  | Arrow (t1, t2) -> sprintf "(arr %s %s)" (show_typ t1) (show_typ t2)

let show_term : term -> string =
 fun t ->
  let rec helper depth = function
    | Atom (Variable (x, tau)) -> sprintf "V.%s(%s)" x (show_typ tau)
    | Atom (Constant (x, tau)) -> sprintf "C.%s(%s)" x (show_typ tau)
    | Application (t1, t2) ->
        sprintf
          "(%s\n%s%s)"
          (helper depth t1)
          (String.init (4 * (depth + 1)) ~f:(fun _ -> ' '))
          (helper (depth + 1) t2)
    | Abstraction (x, tau, t) ->
        sprintf
          "(lam %s(%s).\n%s%s)"
          x
          (show_typ tau)
          (String.init (4 * (depth + 1)) ~f:(fun _ -> ' '))
          (helper (depth + 1) t)
  in
  helper 0 t

(* Types *)

let atom_typ : atom -> typ = function
  | Variable (_, typ) -> typ
  | Constant (_, typ) -> typ

let rec typ_consistent : typ -> typ -> bool =
 fun tau1 tau2 ->
  match (tau1, tau2) with
  | Elementary _, Elementary _ -> true
  | Arrow (a, b), Arrow (c, d) -> typ_consistent a c && typ_consistent b d
  | Elementary _, Arrow (_, _) | Arrow (_, _), Elementary _ -> false

let rec typ : term -> typ = function
  | Atom atom -> atom_typ atom
  | Application (t1, t2) ->
      (match typ t1 with
      | Arrow (alpha, beta) ->
          if typ_consistent (typ t2) alpha
          then beta
          else
            failwith
              (sprintf
                 "argument (%s) type mismatch: %s and %s"
                 ([%show: term] t2)
                 ([%show: typ] (typ t2))
                 ([%show: typ] alpha))
      | tau ->
          failwith
            (sprintf
               "head of type %s (not arrow type): %s applied to %s"
               ([%show: typ] tau)
               ([%show: term] t1)
               ([%show: term] t2)))
  | Abstraction (param, alpha, body) -> Arrow (alpha, typ body)

(* Substitution function *)

let gensym_prefix : string = "univar"

let rec free_variables : term -> String.Set.t = function
  | Atom (Variable (x, _)) -> String.Set.singleton x
  | Atom (Constant (c, _)) -> String.Set.empty
  | Application (t1, t2) -> Set.union (free_variables t1) (free_variables t2)
  | Abstraction (param, _, body) -> Set.remove (free_variables body) param

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

let substitute_recursively : (string * term) list -> term -> term =
 fun bindings e ->
  let fvs =
    List.fold_left bindings ~init:String.Set.empty ~f:(fun acc (_, rhs) ->
        free_variables rhs)
  in
  let rec recurse = function
    | Atom (Variable (x, alpha)) ->
        (match List.Assoc.find ~equal:String.equal bindings x with
        | Some rhs -> recurse rhs
        | None -> Atom (Variable (x, alpha)))
    | Atom (Constant (c, alpha)) -> Atom (Constant (c, alpha))
    | Application (t1, t2) -> Application (recurse t1, recurse t2)
    | Abstraction (param, alpha, body) ->
        (match List.Assoc.find ~equal:String.equal bindings param with
        | Some _ -> Abstraction (param, alpha, body)
        | None ->
            if not (Set.mem fvs param)
            then Abstraction (param, alpha, recurse body)
            else (
              let new_param = Util.gensym gensym_prefix in
              Abstraction
                (new_param, alpha, recurse (replace (param, new_param) body))))
  in
  recurse e

let substitute : string * term -> term -> term =
 fun binding e -> substitute_recursively [ binding ] e

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
    | _ -> failwith (sprintf "non-normalized head: %s" (show_term t))
  in
  let domain, codomain = decompose_arr (atom_typ head) in
  let eta_ws =
    List.map
      ~f:(fun alpha -> (Util.gensym gensym_prefix, alpha))
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

type substitution_pair = string * term [@@deriving sexp, ord]
type substitution = substitution_pair list [@@deriving sexp, ord]

(* === The unification algorithm ===  *)

(* Types *)

type disagreement_set = (term * term) list [@@deriving sexp, ord]

type matching_tree =
  | Terminal of bool
  | Nonterminal of disagreement_set * (substitution_pair * matching_tree) list
[@@deriving sexp, ord]

(* SIMPL procedure *)

let headings_equal
    : (string * typ) list * atom -> (string * typ) list * atom -> bool
  =
 fun (binding1, head1) (binding2, head2) ->
  Int.equal (List.length binding1) (List.length binding2)
  &&
  match
    ( head1
    , normalize
        (build_applications
           (build_abstractions binding2 (Atom head2))
           (List.map ~f:(fun (x, alpha) -> Atom (Variable (x, alpha))) binding1))
    )
  with
  | Variable (x1, _), Atom (Variable (x2, _)) -> String.equal x1 x2
  | Constant (x1, _), Atom (Constant (x2, _)) ->
      String.equal x1 x2
      ||
      (match (Util.unembed_name x1, Util.unembed_name x2) with
      | Some (prefix1, metadata1), Some (prefix2, metadata2) ->
          String.equal prefix1 prefix2 && String.equal metadata1 metadata2
      | _ -> false)
  | Variable _, _ | Constant _, _ -> false

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
        (* TODO: Conceptually, this should never fail; however, it may actually
           fail for polymorphic constructs like catamorphisms that can take in
           different numbers of parameters even for the same datatype depending
           on the result type. *)
        match
          List.map2 argument1 argument2 ~f:(fun e1 e2 ->
              (build_abstractions binding1 e1, build_abstractions binding2 e2))
        with
        | Base.List.Or_unequal_lengths.Ok new_ds ->
            simpl_step1 (new_ds @ rest_ds)
        | Base.List.Or_unequal_lengths.Unequal_lengths -> None)
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
  let domain, codomain = decompose_arr head1_typ in
  assert (Int.equal p1 (List.length domain));
  let should_imitate =
    match head2 with
    | Constant _ -> true
    | Variable (_, _) -> false
  in
  let ws =
    List.map ~f:(fun alpha -> (Util.gensym gensym_prefix, alpha)) domain
  in
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
                    let h = Util.gensym gensym_prefix in
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
        if typ_consistent w_codomain codomain
        then
          Some
            ( head1_var
            , build_abstractions
                ws
                (build_applications
                   (Atom (Variable (w, w_typ)))
                   (List.map w_domain ~f:(fun w_arg_typ ->
                        let h = Util.gensym gensym_prefix in
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
