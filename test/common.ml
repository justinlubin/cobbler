open Core
open Lib
open Lang

let parse_file : string -> typ_env * env * exp =
 fun filename ->
  In_channel.with_file ("test_data/" ^ filename) ~f:(fun file ->
      Parse.program (In_channel.input_all file))
