
val gcd : int -> int -> int
val lcm : int -> int -> int

val num_of_digits : int -> int
val is_permutation : int -> int -> bool
val bit_length : int -> int
val bin_of_int : int -> string
val isqrt : int -> int

val is_triangular : int -> bool
val is_square : int -> bool
val is_pentagonal : int -> bool
val is_hexagonal : int -> bool

val divisors : int -> int list
val factorize : int -> (int * int) list
val pfactors_to_divisors : (int * int) list -> int list

val is_pandigital : int -> bool
val is_pandigital_str : string -> bool
val is_pandigital_strlst : string list -> bool
val is_pandigital_lst : int list -> bool

val is_palindrome : int -> bool

(* naive method *)
val is_prime : int -> bool
(* Miller–Rabin primality test *)
val mr_isprime : int -> bool

module Prime : sig
  type t

  (* prime table, minimum factor table, mobius table *)
  val make_tables : int -> t
  val is_prime : t -> int -> bool
  val prime_tbl : t -> bool array
  val minfactor_tbl : t -> int array
  val mobius_tbl : t -> int array
  val prime_list : t -> int list

  val factorize : t -> int -> (int * int) list
  val divisors : t -> int -> int list

  val generator : unit -> unit -> int
  val generator2 : ?start:int -> unit -> unit -> int
end
