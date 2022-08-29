open Core
open Lib
open Lang

let parse_file : string -> typ_env * env =
 fun filename ->
  In_channel.with_file filename ~f:(fun file ->
      Parse.definitions (In_channel.input_all file))

let problem =
  Synthesis.problem_of_definitions
    (parse_file "test/test_data/programs/classic.lisp")

let () =
  print_endline "beginning synthesis...";
  (match Synthesis.solve problem with
  | None -> print_endline "no solution found"
  | Some e -> printf "solution found:\n%s\n" (Lang_util.show_exp e));
  print_endline "have a nice day!"
