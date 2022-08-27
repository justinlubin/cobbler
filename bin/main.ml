open Core
open Lib
open Lang

let parse_file : string -> env * exp =
 fun filename ->
  In_channel.with_file ("test/" ^ filename) ~f:(fun file ->
      Parse.program (In_channel.input_all file))

let env, exp = parse_file "test_programs/classic.lisp"

let () =
  print_endline "beginning synthesis...";
  match Synthesize.synthesize env exp with
  | None -> print_endline "no solution found"
  | Some e -> printf "solution found:\n%s\n" (Lang_util.show_exp e)
