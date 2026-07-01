(* Various functions *)
val gcd : int -> int -> int
val lcm : int -> int -> int
val binomial : int -> int -> int
val int_pow : int -> int -> int
val powmod : int -> int -> int -> int
val get_max_exp : ?base:int -> int -> int
val num_of_digits : ?base:int -> int -> int
val isqrt : int -> int
val jacobi_symbol : int -> int -> int

(* Divisor and Prime factorization *)
val factorize : int -> (int * int) list
val pfactors_to_divisors : (int * int) list -> int list
val divisors : int -> int list

(* Polygonal number test *)
val is_triangular : int -> bool
val is_square : int -> bool
val is_pentagonal : int -> bool
val is_hexagonal : int -> bool

(* pandigital number test *)
val is_pandigital : int -> bool
val is_pandigital_nz : int -> bool
val is_palindrome : ?base:int -> int -> bool
val is_permutation : int -> int -> bool

(* Functions related to prime numbers *)
module Prime : sig
  val is_prime : int -> bool
  val fermat_prime : int -> bool
  val next_prime : int -> int
  val prev_prime : int -> int
  val primes : int -> int -> int list
end

(* Divisor function *)
val sigma_tbl : int -> int -> int array
val aliquot_sum_tbl : int -> int array
