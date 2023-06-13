open Core
open Cbr_fp
open Lang

let () =
  let stdlib_fname = "../../../benchmark_fp/Stdlib.json" in
  let sigma, gamma, env =
    In_channel.with_file stdlib_fname ~f:(fun file ->
        Cbr_fp.Parse_json.definitions (In_channel.input_all file))
  in
  let input = In_channel.(input_line_exn stdin) in
  let name, typ, var = Parse_json.variable_definition input in
  let env = Map.add_exn env ~key:name ~data:var in
  let gamma = Map.add_exn gamma ~key:name ~data:typ in
  (* print_endline "parsed!";
  printf "type checking... %!";
  Type_system.well_typed (sigma, gamma, env);
  print_endline "type checked!"; *)
  let problem = Synthesis.problem_of_definitions (sigma, gamma, env) in
  (* printf
    "reference:\n\n%s\n\n"
    (Exp.show_multi 1 (String.Map.find_exn env "main")); *)
  (* printf "synthesizing... %!"; *)
  match Synthesis.solve ~use_unification:true ~depth:5 problem with
  | None -> print_endline "no solution found"
  | Some e ->
      printf "solution found:\n\n%s\n\n" (Exp.show_multi 1 (Exp.clean e));
      printf "of type: %s\n\n" (Typ.show (Type_system.infer sigma gamma e))
