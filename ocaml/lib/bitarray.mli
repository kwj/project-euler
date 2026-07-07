type t

val init : int -> t
val set : t -> int -> unit
val unset : t -> int -> unit
val test : t -> int -> bool
val popcount : t -> int
val all_clear : t -> unit
