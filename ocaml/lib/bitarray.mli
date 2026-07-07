type t

val init : int -> t
val set : t -> int -> unit
val clear : t -> int -> unit
val test : t -> int -> bool
val popcount : t -> int
