let enabled : bool =
  Core.Array.mem
    ~equal:Core.String.equal
    (Core.Sys.get_argv ())
    "--timing-breakdown=true"

type category =
  | Enumeration
  | UnificationOutsideEnumeration
  | UnificationInsideEnumeration
  | CanonicalizationOutsideEnumeration
  | CanonicalizationInsideEnumeration

type overall_category =
  | EnumerationOnly
  | UnificationOnly
  | CanonicalizationOnly

let enum : float ref = ref 0.0
let unif_out : float ref = ref 0.0
let unif_in : float ref = ref 0.0
let canon_out : float ref = ref 0.0
let canon_in : float ref = ref 0.0

let add_time : category -> float -> unit =
 fun cat x ->
  match cat with
  | Enumeration -> enum := !enum +. x
  | UnificationOutsideEnumeration -> unif_out := !unif_out +. x
  | UnificationInsideEnumeration -> unif_in := !unif_in +. x
  | CanonicalizationOutsideEnumeration -> canon_out := !canon_out +. x
  | CanonicalizationInsideEnumeration -> canon_in := !canon_in +. x

let time_taken : overall_category -> float =
 fun cat ->
  match cat with
  | EnumerationOnly -> !enum -. !unif_in -. !canon_in
  | UnificationOnly -> !unif_out +. !unif_in
  | CanonicalizationOnly -> !canon_out +. !canon_in

let record1 cat f =
  if enabled
  then
    fun a ->
    let start = Unix.gettimeofday () in
    Fun.protect
      (fun () -> f a)
      ~finally:(fun () ->
        let stop = Unix.gettimeofday () in
        add_time cat (stop -. start))
  else f

let record3 cat f =
  if enabled
  then
    fun a b c ->
    let start = Unix.gettimeofday () in
    Fun.protect
      (fun () -> f a b c)
      ~finally:(fun () ->
        let stop = Unix.gettimeofday () in
        add_time cat (stop -. start))
  else f

let record4 cat f =
  if enabled
  then
    fun a b c d ->
    let start = Unix.gettimeofday () in
    Fun.protect
      (fun () -> f a b c d)
      ~finally:(fun () ->
        let stop = Unix.gettimeofday () in
        add_time cat (stop -. start))
  else f

let record_thunk cat thunk = record1 cat thunk ()
