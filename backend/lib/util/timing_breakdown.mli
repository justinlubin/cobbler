(** Record the timing breakdown of various components of the algorithm

    **Workaround note**: This module will only record timing breakdowns if the
    flag [--timing-breakdown=true] is passed to the executable. Otherwise, this
    module will simply do nothing (the [recordN] functions will be the identity
    and have no side effects). *)

(** The component categories to time *)
type category =
  | Enumeration
  | UnificationOutsideEnumeration
  | UnificationInsideEnumeration
  | CanonicalizationOutsideEnumeration
  | CanonicalizationInsideEnumeration

(** The overall categories (computed from lower-level categories) *)
type overall_category =
  | EnumerationOnly
  | UnificationOnly
  | CanonicalizationOnly

(** [time_taken cat] returns the time taken on overall category [cat] *)
val time_taken : overall_category -> float

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

(** [record_thunk cat thunk] tracks the time taken to execute [thunk] and adds
    it to the [cat] category if this module is enabled (see module
    documentation); otherwise, it simply executes [thunk]. *)
val record_thunk : category -> (unit -> 'a) -> 'a
