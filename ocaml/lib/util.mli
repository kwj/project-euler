val permutation : int -> 'a list -> 'a list list
val permutation_with_repetition : int -> 'a list -> 'a list list
val combination : int -> 'a list -> 'a list list
val combination_with_repetition : int -> 'a list -> 'a list list
val powerset : 'a list -> 'a list list
val findall : ('a -> bool) -> 'a list -> int list

(* popcount *)
val popcount_64 : int64 -> int
val popcount_32 : int32 -> int
val popcount_int : int -> int
val popcount : int -> int
val popcount_char : char -> int
val popcount_nativeint : nativeint -> int

(* Number of Trailing Zero (NTZ) *)
val get_NTZ_64 : int64 -> int
val get_NTZ_32 : int32 -> int
val get_NTZ_int : int -> int
val get_NTZ : int -> int
val get_NTZ_char : char -> int
val get_NTZ_nativeint : nativeint -> int

(* misc. *)
val list_to_str : ('a -> string) -> string -> 'a list -> string
val list_assoc_group : ('a * 'b) list -> ('a * 'b list) list

(* from Julia *)
val digits : ?base:int -> int -> int list
val undigits : ?base:int -> int list -> int
val z_digits : ?base:int -> Z.t -> int list
val z_undigits : ?base:int -> int list -> Z.t
