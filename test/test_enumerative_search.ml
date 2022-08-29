open Core
open Lib

module Calc = struct
  module Exp = struct
    module T = struct
      type t =
        | Zero
        | One
        | In
        | Negate of t
        | Plus of t * t
        | Times of t * t
        | IfNonNeg of t * t * t
      [@@deriving sexp, quickcheck, ord, compare]
    end

    include T
    include Comparator.Make (T)
  end

  open Exp

  let rec eval (input : int) (e : Exp.t) : int =
    match e with
    | Zero -> 0
    | One -> 1
    | In -> input
    | Negate e1 -> -eval input e1
    | Plus (e1, e2) -> eval input e1 + eval input e2
    | Times (e1, e2) -> eval input e1 * eval input e2
    | IfNonNeg (e1, e2, e3) ->
        if eval input e1 >= 0 then eval input e2 else eval input e3

  let obs_eq (inputs : int list) (e1 : Exp.t) (e2 : Exp.t) : bool =
    List.for_all ~f:(fun i -> eval i e1 = eval i e2) inputs

  let prune_obs_eq (inputs : int list) (es : Exp.t list) : Exp.t list =
    Lib.Util.dedup_by ~f:(obs_eq inputs) es

  let grow
      (examples : (int * int) list)
      (space : (Exp.t, Exp.comparator_witness) Set.t)
      : (Exp.t, Exp.comparator_witness) Enumerative_search.grow_result
    =
    let open List.Let_syntax in
    let plist = Set.to_list space in
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
    let additions =
      expansion1 @ expansion2 @ expansion3
      |> prune_obs_eq (List.map ~f:fst examples)
      |> Set.of_list (module Exp)
    in
    let new_space = Set.union space additions in
    Enumerative_search.{ additions; new_space }

  let satisfies_examples (examples : (int * int) list) (e : Exp.t) : bool =
    List.for_all ~f:(fun (input, output) -> eval input e = output) examples

  let enumerate (examples : (int * int) list) : Exp.t option =
    Lib.Enumerative_search.search
      ~max_iterations:2
      ~initial_space:(Set.of_list (module Exp) [ Zero; One; In ])
      ~grow:(grow examples)
      ~correct:(satisfies_examples examples)

  module Test = struct
    let%test_unit "basic calculator example 1" =
      [%test_eq: Exp.t option]
        (enumerate [ (0, 1); (4, 5) ])
        (Some (Plus (One, In)))

    let%test_unit "basic calculator example 2" =
      [%test_eq: Exp.t option] (enumerate [ (0, 0); (0, 1) ]) None

    let%test_unit "basic calculator example 3" =
      [%test_eq: Exp.t option]
        (enumerate [ (2, 5); (3, 10); (4, 17) ])
        (Some (Plus (One, Times (In, In))))
  end
end
