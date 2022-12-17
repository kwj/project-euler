
type t

val generate : int -> t
val is_prime : t -> int -> bool
val count : t -> int
val to_list : t -> int list
val to_array : t -> int array
val prev_prime : t -> int -> int
