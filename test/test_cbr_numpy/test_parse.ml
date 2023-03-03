open Cbr_numpy.Parse
open Core

let input_test1 = "x = 3 + 5"
let input_test2 = "y = 2 * 5"

let expected_test1 =
  "((body((Assign(location((start((line 1)(column 0)))(stop((line 1)(column \
   9)))))(targets((Name(location((start((line 1)(column 0)))(stop((line \
   1)(column 1)))))(id x)(ctx Store))))(value(BinOp(location((start((line \
   1)(column 4)))(stop((line 1)(column \
   9)))))(left(Constant(location((start((line 1)(column 4)))(stop((line \
   1)(column 5)))))(value(Integer 3))(kind())))(op \
   Add)(right(Constant(location((start((line 1)(column 8)))(stop((line \
   1)(column 9)))))(value(Integer \
   5))(kind())))))(type_comment()))))(type_ignores()))"

let expected_test2 =
  "((body((Assign(location((start((line 1)(column 0)))(stop((line 1)(column \
   9)))))(targets((Name(location((start((line 1)(column 0)))(stop((line \
   1)(column 1)))))(id y)(ctx Store))))(value(BinOp(location((start((line \
   1)(column 4)))(stop((line 1)(column \
   9)))))(left(Constant(location((start((line 1)(column 4)))(stop((line \
   1)(column 5)))))(value(Integer 2))(kind())))(op \
   Mult)(right(Constant(location((start((line 1)(column 8)))(stop((line \
   1)(column 9)))))(value(Integer \
   5))(kind())))))(type_comment()))))(type_ignores()))"

let parsed_test1 : string = parse_py input_test1 |> str_of_env
let parsed_test2 : string = parse_py input_test2 |> str_of_env

let%test_unit "parse program 1" =
  [%test_result: string] parsed_test1 ~expect:expected_test1

let%test_unit "parse program 2" =
  [%test_result: string] parsed_test2 ~expect:expected_test2
