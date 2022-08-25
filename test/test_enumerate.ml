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
  [@@deriving sexp, quickcheck]

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
      ~max_iterations:3
      ~grow
      ~prune:(prune_obs_eq (List.map ~f:fst examples))
      ~is_correct:(satisfies_examples examples)

  module Test = struct
    let reasonable_num = Int.gen_incl (-9) 9

    let reasonable_examples =
      Quickcheck.Generator.list
        (Quickcheck.Generator.both reasonable_num reasonable_num)

    let%test_unit "calculator enumeration sound" =
      Quickcheck.test
        ~sexp_of:[%sexp_of: (int * int) list]
        reasonable_examples
        ~f:(fun examples ->
          match enumerate examples with
          | None -> ()
          | Some prog -> [%test_pred: exp] (satisfies_examples examples) prog)

    let%test_unit "random calculator program can be synthesized" =
      Quickcheck.test
        ~sexp_of:[%sexp_of: exp]
        [%quickcheck.generator: exp]
        ~f:(fun reference_prog ->
          let examples =
            List.map
              ~f:(fun i -> i, eval i reference_prog)
              (List.range ~start:`inclusive ~stop:`inclusive (-5) 5)
          in
          match enumerate examples with
          | None -> failwith "Failed to synthesize a program for examples"
          | Some prog -> ())
  end
end
