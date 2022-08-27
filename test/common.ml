open Core
open Lib
open Lang

let parse_file : string -> env * exp =
 fun filename ->
  In_channel.with_file ("../../../test/" ^ filename) ~f:(fun file ->
      Parse.program (In_channel.input_all file))
