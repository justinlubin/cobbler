open Core
open Cbr_framework

module Calc = struct
  type t =
    | Zero
    | One
    | In
    | Negate of t
    | Plus of t * t
    | Times of t * t
    | IfNonNeg of t * t * t
  [@@deriving sexp, ord]

  let rec eval (input : int) (e : t) : int =
    match e with
    | Zero -> 0
    | One -> 1
    | In -> input
    | Negate e1 -> -eval input e1
    | Plus (e1, e2) -> eval input e1 + eval input e2
    | Times (e1, e2) -> eval input e1 * eval input e2
    | IfNonNeg (e1, e2, e3) ->
        if eval input e1 >= 0 then eval input e2 else eval input e3

  let obs_eq (inputs : int list) (e1 : t) (e2 : t) : bool =
    List.for_all ~f:(fun i -> eval i e1 = eval i e2) inputs

  let prune_obs_eq (inputs : int list) (es : t list) : t list =
    Util.dedup_by ~f:(obs_eq inputs) es

  let grow (inputs : int list) (space : t list) : t list =
    let open List.Let_syntax in
    let expansion1 =
      let%bind e1 = space in
      [ Negate e1 ]
    in
    let expansion2 =
      let%bind e1 = space in
      let%bind e2 = space in
      [ Plus (e1, e2); Times (e1, e2) ]
    in
    let expansion3 =
      let%bind e1 = space in
      let%bind e2 = space in
      let%bind e3 = space in
      [ IfNonNeg (e1, e2, e3) ]
    in
    prune_obs_eq inputs (space @ expansion1 @ expansion2 @ expansion3)

  let satisfies_examples (examples : (int * int) list) (e : t) : bool =
    List.for_all ~f:(fun (input, output) -> eval input e = output) examples

  let enumerate (examples : (int * int) list) : t option =
    Enumerative_search.bottom_up
      ~max_iterations:2
      ~initial_candidates:
        (prune_obs_eq (List.map ~f:fst examples) [ Zero; One; In ])
      ~grow:(grow (List.map ~f:fst examples))
      ~correct:(satisfies_examples examples)

  module Test = struct
    let%test_unit "basic calculator example 1" =
      [%test_eq: t option]
        (enumerate [ (0, 1); (4, 5) ])
        (Some (Plus (One, In)))

    let%test_unit "basic calculator example 2" =
      [%test_eq: t option] (enumerate [ (0, 0); (0, 1) ]) None

    let%test_unit "basic calculator example 3" =
      [%test_eq: t option]
        (enumerate [ (2, 5); (3, 10); (4, 17) ])
        (Some (Plus (One, Times (In, In))))
  end
end
