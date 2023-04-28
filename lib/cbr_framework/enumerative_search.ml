open Core

let bottom_up :
      'e.
      max_iterations:int
      -> initial_candidates:'e list
      -> grow:('e list -> 'e list)
      -> correct:('e -> bool)
      -> 'e option
  =
 fun ~max_iterations ~initial_candidates ~grow ~correct ->
  let rec bottom_up' iterations candidates =
    if iterations >= max_iterations
    then None
    else (
      let new_candidates = grow candidates in
      match List.find ~f:correct new_candidates with
      | Some e -> Some e
      | None -> bottom_up' (iterations + 1) new_candidates)
  in
  bottom_up' 0 initial_candidates

let top_down :
      'e.
      max_iterations:int
      -> start:'e
      -> expand:(int -> 'e -> 'e list)
      -> correct:('e -> 'e option)
      -> 'e option
  =
 fun ~max_iterations ~start ~expand ~correct ->
  let rec top_down' iterations candidates =
    if iterations >= max_iterations
    then None
    else (
      let new_candidates = List.concat_map ~f:(expand iterations) candidates in
      match List.find_map ~f:correct new_candidates with
      | Some e -> Some e
      | None -> top_down' (iterations + 1) new_candidates)
  in
  top_down' 0 [ start ]
