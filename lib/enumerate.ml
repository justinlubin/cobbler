open Core

let search :
      'e.
      max_iterations:int
      -> terminals:'e list
      -> grow:('e list -> 'e list)
      -> prune:('e list -> 'e list)
      -> is_correct:('e -> bool)
      -> 'e option
  =
 fun ~max_iterations ~terminals ~grow ~prune ~is_correct ->
  let rec search' iter plist =
    if iter >= max_iterations
    then None
    else (
      let plist' = prune (grow plist) in
      match List.find ~f:is_correct plist' with
      | Some e -> Some e
      | None -> search' (iter + 1) plist')
  in
  search' 0 terminals
