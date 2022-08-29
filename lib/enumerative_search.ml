open Core

let search :
      'e.
      max_iterations:int
      -> initial_space:'e list
      -> grow:('e list -> 'e list)
      -> correct:('e -> bool)
      -> 'e option
  =
 fun ~max_iterations ~initial_space ~grow ~correct ->
  let rec search' iterations space =
    if iterations >= max_iterations
    then None
    else (
      let new_space = grow space in
      match List.find ~f:correct new_space with
      | Some e -> Some e
      | None -> search' (iterations + 1) new_space)
  in
  search' 0 initial_space
