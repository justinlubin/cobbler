(** Record the timing breakdown of various components of the algorithm 

    **Note**: This module will only record time breakdowns if the "enabled" is
    set to [true] in the implementation of this module. Otherwise, it will
    simply do nothing (the record functions will be the identity).
 *)

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
