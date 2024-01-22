open Lang
open Env
open Partial_eval_trace
open Inline

let rec get_candidates : expr -> expr list =
 fun e ->
  match e with
  | Num _ -> []
  | Str _ -> []
  | Name id -> [Name id]
  | Hole _ -> []
  | Index (e1, e2) -> Index (e1, e2) :: get_candidates e1 @ get_candidates e2
  | Call (e1, e_list) -> Call (e1, e_list) :: get_candidates e1 @ List.concat_map get_candidates e_list

let make_trace_entry : expr -> trace_entry =
 fun e ->
  let p = Env.np_env, [Return e] in
  let _, t = (p, [p]) |> inline_program_with_trace |> partial_eval_program_with_trace in
  p, t
  
let make_trace : program -> trace =(**
  let rec make_trace' candidates =
    let traces, new_candidates = List.map make_trace_entry candidates |> List.split in
    match new_candidates with
    | [] -> traces
    | _ -> traces @ make_trace' new_candidates
  in *)
 fun (_, block) ->
  match block with
  | [Return e] -> List.map make_trace_entry (get_candidates e)
  | _ -> []
