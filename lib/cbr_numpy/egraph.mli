open Ego.Generic
module L : LANGUAGE
module A : ANALYSIS

module MA : functor
  (_ : GRAPH_API
         with type 'p t = (Ego.Id.t L.shape, A.t, A.data, 'p) egraph
          and type analysis := A.t
          and type data := A.data
          and type 'a shape := 'a L.shape
          and type node := L.t)
  ->
  ANALYSIS_OPS
    with type 'p t = (Ego.Id.t L.shape, A.t, A.data, 'p) egraph
     and type analysis := A.t
     and type data := A.data
     and type node := Ego.Id.t L.shape

module EGraph : sig
  type 'p t = (Ego.Id.t L.shape, unit, unit, 'p) egraph

  module Rule : sig
    type t = Ego.Generic.Make(L)(A)(MA).Rule.t

    val make_constant : from:L.op Query.t -> into:L.op Query.t -> t

    val make_conditional
      :  from:L.op Query.t
      -> into:L.op Query.t
      -> cond:
           ((Ego.Id.t L.shape, unit, unit, rw) egraph
            -> Ego.Id.t
            -> Ego.Id.t StringMap.t
            -> bool)
      -> t

    val make_dynamic
      :  from:L.op Query.t
      -> generator:
           ((Ego.Id.t L.shape, unit, unit, rw) egraph
            -> Ego.Id.t
            -> Ego.Id.t StringMap.t
            -> L.op Query.t option)
      -> t
  end

  val freeze : rw t -> ro t
  val init : unit -> 'p t
  val class_equal : ro t -> Ego.Id.t -> Ego.Id.t -> bool
  val set_data : rw t -> Ego.Id.t -> unit -> unit
  val get_data : 'a t -> Ego.Id.t -> unit
  val get_analysis : rw t -> unit
  val iter_children : ro t -> Ego.Id.t -> Ego.Id.t L.shape Iter.t
  val to_dot : (Ego.Id.t L.shape, unit, unit, 'a) egraph -> Odot.graph
  val add_node : rw t -> L.t -> Ego.Id.t
  val merge : rw t -> Ego.Id.t -> Ego.Id.t -> unit
  val rebuild : rw t -> unit

  val find_matches
    :  ro t
    -> L.op Query.t
    -> (Ego.Id.t * Ego.Id.t StringMap.t) Iter.t

  val apply_rules
    :  (Ego.Id.t L.shape, unit, unit, rw) egraph
    -> Rule.t list
    -> unit

  val run_until_saturation
    :  ?scheduler:Scheduler.Backoff.t
    -> ?node_limit:[ `Bounded of int | `Unbounded ]
    -> ?fuel:[ `Bounded of int | `Unbounded ]
    -> ?until:((Ego.Id.t L.shape, unit, unit, rw) egraph -> bool)
    -> (Ego.Id.t L.shape, unit, unit, rw) egraph
    -> Rule.t list
    -> bool

  module BuildRunner : functor
    (S : sig
       type t
       type data

       val default : unit -> t
       val should_stop : t -> int -> data Iter.t -> bool
       val create_rule_metadata : t -> Rule.t -> data

       val guard_rule_usage
         :  (Ego.Id.t L.shape, unit, unit, rw) egraph
         -> t
         -> data
         -> int
         -> (unit -> (Ego.Id.t * Ego.Id.t StringMap.t) Iter.t)
         -> (Ego.Id.t * Ego.Id.t StringMap.t) Iter.t
     end)
    -> sig
    val run_until_saturation
      :  ?scheduler:S.t
      -> ?node_limit:[ `Bounded of int | `Unbounded ]
      -> ?fuel:[ `Bounded of int | `Unbounded ]
      -> ?until:((Ego.Id.t L.shape, unit, unit, rw) egraph -> bool)
      -> (Ego.Id.t L.shape, unit, unit, rw) egraph
      -> Rule.t list
      -> bool
  end
end

val op_of_string : string -> L.op
val string_of_op : L.op -> string
val t_of_sexp : Sexplib.Sexp.t -> L.t
