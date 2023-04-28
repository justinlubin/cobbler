open Core
open Cbr_fp
open Cbr_numpy
open Cbr_fp.Lang
open Cbr_numpy.Lang
open Cbr_numpy.Unification

let () =
  Core.Caml.Printexc.register_printer (function
      | Type_system.IllTyped e ->
          Some (Printf.sprintf "IllTyped(%s)" (Exp.show_multi 0 e))
      | _ -> None)

let parse_file_fp : string -> datatype_env * typ_env * Cbr_fp.Lang.env =
 fun filename ->
  In_channel.with_file filename ~f:(fun file ->
      Cbr_fp.Parse.definitions (In_channel.input_all file))

let parse_file_np : string -> program =
 fun filename ->
  In_channel.with_file filename ~f:(fun file ->
      Sexp.of_string (In_channel.input_all file) |> Parse.program_of_sexp)

let file = "test/test_cbr_fp/test_data/programs/list2.lisp"

let np_prog_fname =
  "test/test_cbr_numpy/test_data/programs/test_simple_comp.sexp"

let np_pat_fname = "test/test_cbr_numpy/test_data/programs/test_simple.sexp"

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
  print_endline "Starting NumPy execution:";
  print_endline "Parsing ... %!";
  let target = parse_file_np np_prog_fname in
  let pattern = parse_file_np np_pat_fname in
  let subs = unify_egraph ~debug:true ~target ~pattern () in
  (match subs with
  | None -> print_endline "failure"
  | Some subs ->
      print_endline
        (Cbr_numpy.Parse.sexp_of_substitutions subs |> Sexp.to_string));
  print_endline "\ndone!"
