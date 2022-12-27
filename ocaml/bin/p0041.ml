(* Project Euler: Problem 41 *)

(*
  (sum_{1}_{9} n) mod 3 = 45 mod 3 = 0  --> 9 digits pandigital number is a multiple of 3, not a prime.
  (sum_{1}_{8} n) mod 3 = 36 mod 3 = 0  --> 8 digits pandigital number is a multiple of 3, not a prime.
  (sum_{1}_{7} n) mod 3 = 28 mod 3 = 1
  (sum_{1}_{6} n) mod 3 = 21 mod 3 = 0  --> 6 digits pandigital number is a multiple of 3, not a prime.
  (sum_{1}_{5} n) mod 3 = 15 mod 3 = 0  --> 5 digits pandigital number is a multiple of 3, not a prime.
  (sum_{1}_{4} n) mod 3 = 10 mod 3 = 1
 *)

open Core

let solve max_num =
  let is_pandigital = Euler.Math.is_pandigital_nz in
  let is_prime = Euler.Math.mr_isprime in

  (* This function always terminates. Because n=2143 is a 4-digit pandigital and is also prime. *)
  let rec aux n =
    if n > 7_654_321 then
      aux 7_654_321
    else if n = 999_999 then
      aux 9_999
    else
      if is_pandigital n && is_prime n then
        n
      else
        aux (n - 2)
  in
  let num = if max_num mod 2 = 0 then max_num - 1 else max_num in
  aux num

let exec () =
  Int.to_string (solve 7_654_321)

let () = Euler.Task.run exec
