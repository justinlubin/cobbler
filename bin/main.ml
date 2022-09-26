open Core
open Lib
open Lang

let parse_file : string -> datatype_env * typ_env * env =
 fun filename ->
  In_channel.with_file filename ~f:(fun file ->
      Parse.definitions (In_channel.input_all file))

let problem =
  let _, gamma, env = parse_file "test/test_data/programs/classic.lisp" in
  Synthesis.problem_of_definitions (gamma, env)

let () =
  print_endline "beginning synthesis...";
  (match Synthesis.solve problem with
  | None -> print_endline "no solution found"
  | Some e -> printf "solution found:\n%s\n" (Exp.show e));
  print_endline "have a nice day!"
