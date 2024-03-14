let enabled : bool =
  Core.Array.mem
    ~equal:Core.String.equal
    (Core.Sys.get_argv ())
    "--timing-breakdown=true"

type category =
  | Synthesis
  | Unification
  | Canonicalization

let synth : float ref = ref 0.0
let unif : float ref = ref 0.0
let canon : float ref = ref 0.0

let add_time : category -> float -> unit =
 fun cat x ->
  match cat with
  | Synthesis -> synth := !synth +. x
  | Unification -> unif := !unif +. x
  | Canonicalization -> canon := !canon +. x

let time_taken : category -> float =
 fun cat ->
  match cat with
  | Synthesis -> !synth
  | Unification -> !unif
  | Canonicalization -> !canon

let record1 cat f =
  if enabled
  then
    fun a ->
    let start = Unix.gettimeofday () in
    let res = f a in
    let stop = Unix.gettimeofday () in
    let () = add_time cat (stop -. start) in
    res
  else f

let record3 cat f =
  if enabled
  then
    fun a b c ->
    let start = Unix.gettimeofday () in
    let res = f a b c in
    let stop = Unix.gettimeofday () in
    let () = add_time cat (stop -. start) in
    res
  else f

let record4 cat f =
  if enabled
  then
    fun a b c d ->
    let start = Unix.gettimeofday () in
    let res = f a b c d in
    let stop = Unix.gettimeofday () in
    let () = add_time cat (stop -. start) in
    res
  else f
