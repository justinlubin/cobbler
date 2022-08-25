open Core
open Lib

module Calc = struct
  type exp =
    | Zero
    | One
    | In
    | Negate of exp
    | Plus of exp * exp
    | Times of exp * exp
    | IfNonNeg of exp * exp * exp
  [@@deriving sexp, quickcheck, ord]

  let rec eval (input : int) (e : exp) : int =
    match e with
    | Zero -> 0
    | One -> 1
    | In -> input
    | Negate e1 -> -eval input e1
    | Plus (e1, e2) -> eval input e1 + eval input e2
    | Times (e1, e2) -> eval input e1 * eval input e2
    | IfNonNeg (e1, e2, e3) ->
        if eval input e1 >= 0 then eval input e2 else eval input e3

  let grow (plist : exp list) : exp list =
    let open List.Let_syntax in
    let expansion1 =
      let%bind e1 = plist in
      [ Negate e1 ]
    in
    let expansion2 =
      let%bind e1 = plist in
      let%bind e2 = plist in
      [ Plus (e1, e2); Times (e1, e2) ]
    in
    let expansion3 =
      let%bind e1 = plist in
      let%bind e2 = plist in
      let%bind e3 = plist in
      [ IfNonNeg (e1, e2, e3) ]
    in
    plist @ expansion1 @ expansion2 @ expansion3

  let obs_eq (inputs : int list) (e1 : exp) (e2 : exp) : bool =
    List.for_all ~f:(fun i -> eval i e1 = eval i e2) inputs

  let prune_obs_eq (inputs : int list) (es : exp list) : exp list =
    Lib.Util.dedup_by ~f:(obs_eq inputs) es

  let satisfies_examples (examples : (int * int) list) (e : exp) : bool =
    List.for_all ~f:(fun (input, output) -> eval input e = output) examples

  let enumerate (examples : (int * int) list) : exp option =
    Lib.Enumerate.search
      ~max_iterations:2
      ~terminals:[ Zero; One; In ]
      ~grow
      ~prune:(prune_obs_eq (List.map ~f:fst examples))
      ~is_correct:(satisfies_examples examples)

  module Test = struct
    let%test_unit "basic calculator example 1" =
      [%test_eq: exp option] (enumerate [ 0, 1; 4, 5 ]) (Some (Plus (One, In)))

    let%test_unit "basic calculator example 2" =
      [%test_eq: exp option] (enumerate [ 0, 0; 0, 1 ]) None

    let%test_unit "basic calculator example 3" =
      [%test_eq: exp option]
        (enumerate [ 2, 5; 3, 10; 4, 17 ])
        (Some (Plus (One, Times (In, In))))
  end
end
