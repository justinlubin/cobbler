open Core
open Lib
open Lang

let () =
  Core.Caml.Printexc.register_printer (function
      | Type_system.IllTyped e ->
          Some (Printf.sprintf "IllTyped(%s)" (Exp.show_multi 0 e))
      | _ -> None)

let parse_file : string -> datatype_env * typ_env * env =
 fun filename ->
  In_channel.with_file filename ~f:(fun file ->
      Parse.definitions (In_channel.input_all file))

let file = "test/test_data/programs/list2.lisp"

let () =
  print_endline (sprintf "operating on file '%s'" file);
  printf "parsing... %!";
  let sigma, gamma, env = parse_file file in
  print_endline "parsed!";
  printf "type checking... %!";
  Type_system.well_typed (sigma, gamma, env);
  print_endline "type checked!";
  let problem = Synthesis.problem_of_definitions (sigma, gamma, env) in
  printf
    "reference:\n\n%s\n\n"
    (Exp.show_multi 1 (String.Map.find_exn env "main"));
  printf "synthesizing... %!";
  (match Synthesis.solve ~depth:5 problem with
  | None -> print_endline "no solution found"
  | Some e ->
      printf "solution found:\n\n%s\n\n" (Exp.show_multi 1 (Exp.clean e));
      printf "of type: %s\n\n" (Typ.show (Type_system.infer sigma gamma e)));
  print_endline "have a nice day!"
