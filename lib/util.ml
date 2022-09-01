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

let find_and_remove_first :
      'a. 'a list -> f:('a -> bool) -> ('a * 'a list) option
  =
 fun xs ~f ->
  let rec helper acc = function
    | [] -> None
    | hd :: tl ->
        if f hd then Some (hd, List.rev acc @ tl) else helper (hd :: acc) tl
  in
  helper [] xs
