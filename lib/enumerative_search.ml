open Core

type ('e, 'comp) grow_result =
  { additions : ('e, 'comp) Set.t
  ; new_space : ('e, 'comp) Set.t
  }

let search :
      'e.
      max_iterations:int
      -> initial_space:('e, 'comp) Set.t
      -> grow:(('e, 'comp) Set.t -> ('e, 'comp) grow_result)
      -> correct:('e -> bool)
      -> 'e option
  =
 fun ~max_iterations ~initial_space ~grow ~correct ->
  let rec search' iterations space =
    if iterations >= max_iterations
    then None
    else (
      let { additions; new_space } = grow space in
      match Set.find ~f:correct new_space with
      | Some e -> Some e
      | None -> search' (iterations + 1) new_space)
  in
  search' 0 initial_space
