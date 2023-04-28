open Core
open Cbr_numpy
open Cbr_numpy.Lang
open Cbr_numpy.Parse
open Stdlib
open Sys

let () =
  let target_type =
   match read_line () with
   | "Number" -> Number
   | "Array" -> Array
   | _ -> failwith "Invalid target type" 
   in
  let target = read_line () in
  let synthesized = target |> Sexp.of_string |> Parse.program_of_sexp |> (Cbr_numpy.Np_synthesis.solve 5 target_type) in
  match synthesized with
  | None -> print_endline "no solution found"
  | Some e ->
      print_endline (py_str_of_program e);