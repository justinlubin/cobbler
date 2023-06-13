open Core
open Cbr_fp
open Lang

let parse_file : string -> datatype_env * typ_env * env =
 fun filename ->
  In_channel.with_file ("test_data/" ^ filename) ~f:(fun file ->
      Parse.definitions (In_channel.input_all file))

let parse_file_json : string -> datatype_env * typ_env * env =
 fun filename ->
  In_channel.with_file ("test_data/" ^ filename) ~f:(fun file ->
      Parse_json.definitions (In_channel.input_all file))
