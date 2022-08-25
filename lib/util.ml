open Core

let dedup_by : 'a. 'a list -> f:('a -> 'a -> bool) -> 'a list =
 fun xs ~f ->
  let rec dedup_by' acc = function
    | [] -> List.rev acc
    | hd :: tl ->
        let acc' = if List.exists ~f:(f hd) acc then acc else hd :: acc in
        dedup_by' acc' tl
  in
  dedup_by' [] xs
