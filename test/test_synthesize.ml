open Core
open Lib
open Lang

let parse_file : string -> env * exp =
 fun filename ->
  In_channel.with_file ("../../../test/" ^ filename) ~f:(fun file ->
      Parse.program (In_channel.input_all file))

let%test_unit "classic synth 1" =
  let env, exp = parse_file "test_programs/classic.lisp" in
  [%test_eq: exp option] (Synthesize.synthesize env exp) None
