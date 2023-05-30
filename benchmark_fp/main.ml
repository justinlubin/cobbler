open Core
open Cbr_fp
open Lang

let () =
  let input = In_channel.(input_line_exn stdin) in
  let sigma, gamma, env = Parse_json.definitions input in
  print_endline "parsed!";
  printf "type checking... %!";
  Type_system.well_typed (sigma, gamma, env);
  print_endline "type checked!";
  let problem = Synthesis.problem_of_definitions (sigma, gamma, env) in
  printf
    "reference:\n\n%s\n\n"
    (Exp.show_multi 1 (String.Map.find_exn env "main"));
  printf "synthesizing... %!";
  match Synthesis.solve ~use_unification:true ~depth:5 problem with
  | None -> print_endline "no solution found"
  | Some e ->
      printf "solution found:\n\n%s\n\n" (Exp.show_multi 1 (Exp.clean e));
      printf "of type: %s\n\n" (Typ.show (Type_system.infer sigma gamma e))
