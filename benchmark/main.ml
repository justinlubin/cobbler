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
  let input = read_line () in
  let target =
    input
    |> Sexp.of_string
    |> Parse.program_of_sexp
  in
  match Cbr_numpy.Np_synthesis.solve 5 ~debug:false target_type target true with
  | None -> print_endline "no solution found"
  | Some e -> print_endline (py_str_of_program e)
