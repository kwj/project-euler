
val permutation : int -> 'a list -> 'a list list
val combination : int -> 'a list -> 'a list list

val list_to_num : int list -> int
val num_of_list : int list -> int
val num_to_list : int -> int list
val list_of_num : int -> int list

val int_to_nlst : int -> int list
val nlst_of_int : int -> int list
val nlst_to_int : int list -> int
val int_of_nlst : int list -> int

val cmp_nlst : int list -> int list -> int
val add_nlst : int list -> int list -> int list
val sub_nlst : int list -> int list -> int list
val mul_nlst : int list -> int -> int list
val trim_zero_nlst : int list -> int list

val popcount_64 : int64 -> int
val popcount_32 : int32 -> int
val popcount_int : int -> int
val popcount : int -> int
val popcount_char : char -> int
val popcount_nativeint : nativeint -> int

val get_NTZ_64 : int64 -> int
val get_NTZ_32 : int32 -> int
val get_NTZ_int : int -> int
val get_NTZ : int -> int
val get_NTZ_char : char -> int
val get_NTZ_nativeint : nativeint -> int

val list_to_str : ('a -> string) -> string -> 'a list -> string
val list_assoc_group : ('a * 'b) list -> ('a * 'b list) list
