(** Record the timing breakdown of various components of the algorithm

    **Note**: This module will only record time breakdowns if the [enable]
    function is called (once at the start of the program). Otherwise, this
    module will simply do nothing (the [recordN] functions will be the identity
    and have no side effects). *)

(** Enables this module *)
val enable : unit -> unit

(** The component categories to time *)
type category =
  | Synthesis
  | Unification
  | Canonicalization

(** [time_taken cat] returns the time taken on component category [cat] *)
val time_taken : category -> float

(** [record1 cat f] returns [f] augmented with time tracking information for
    [cat] if this module is enabled (see module documentation); otherwise, it
    simply returns [f]. *)
val record1 : category -> ('a -> 'b) -> 'a -> 'b

(** The same as [record1], but with a different number of arguments for [f]! *)
val record3 : category -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd

(** The same as [record1], but with a different number of arguments for [f]! *)
val record4
  :  category
  -> ('a -> 'b -> 'c -> 'd -> 'e)
  -> 'a
  -> 'b
  -> 'c
  -> 'd
  -> 'e
