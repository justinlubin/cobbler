open Core
open Cbr_fp
open Cbr_numpy
open Lang

let () =
  Core.Caml.Printexc.register_printer (function
      | Type_system.IllTyped e ->
          Some (Printf.sprintf "IllTyped(%s)" (Exp.show_multi 0 e))
      | _ -> None)

let parse_file_fp : string -> datatype_env * typ_env * env =
 fun filename ->
  In_channel.with_file filename ~f:(fun file ->
      Cbr_fp.Parse.definitions (In_channel.input_all file))

let parse_file_np : string -> Cbr_numpy.Parse.py_ast =
  fun filename ->
   In_channel.with_file filename ~f:(fun file ->
      Cbr_numpy.Parse.parse_py (In_channel.input_all file))


let file = "test/test_cbr_fp/test_data/programs/list2.lisp"

let file_py = "test/test_cbr_numpy/test_data/programs/test1.py"

let () =
  print_endline "FP execution:";
  print_endline (sprintf "operating on file '%s'" file);
  printf "parsing... %!";
  let sigma, gamma, env = parse_file_fp file in
  print_endline "parsed!";
  printf "type checking... %!";
  Type_system.well_typed (sigma, gamma, env);
  print_endline "type checked!";
  let problem = Synthesis.problem_of_definitions (sigma, gamma, env) in
  printf
    "reference:\n\n%s\n\n"
    (Exp.show_multi 1 (String.Map.find_exn env "main"));
  printf "synthesizing... %!";
  (match Synthesis.solve ~use_unification:true ~depth:5 problem with
  | None -> print_endline "no solution found"
  | Some e ->
      printf "solution found:\n\n%s\n\n" (Exp.show_multi 1 (Exp.clean e));
      printf "of type: %s\n\n" (Typ.show (Type_system.infer sigma gamma e)));
  print_endline "have a nice day!";
  print_endline "Starting NumPy execution:";
  print_endline "Parsing ... %!";
  parse_file_np file_py |> 
  Cbr_numpy.Parse.pprint_ast;
  print_endline "\ndone!";
