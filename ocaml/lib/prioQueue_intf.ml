
module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
  end

module type S =
  sig
    type elt
    type t
    val init : ?n:int -> unit -> t
    val size: t -> int
    val is_empty: t -> bool
    val insert: t -> elt -> unit
    val peek: t -> elt
    val extract: t -> elt
  end

module type PrioQueue =
  sig
    module Make (Ord: OrderedType) : (S with type elt = Ord.t)
  end

