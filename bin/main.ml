open Core
open Lib
open Lang

let parse_file : string -> typ_env * env =
 fun filename ->
  In_channel.with_file filename ~f:(fun file ->
      Parse.definitions (In_channel.input_all file))

let gamma, env, free_vars, goal_typ, main_exp =
  Parse.extract_main_body (parse_file "test/test_data/programs/classic.lisp")

let () =
  print_endline "beginning synthesis...";
  (match Synthesize.synthesize' gamma env free_vars main_exp goal_typ with
  | None -> print_endline "no solution found"
  | Some e -> printf "solution found:\n%s\n" (Lang_util.show_exp e));
  print_endline "have a nice day!"
