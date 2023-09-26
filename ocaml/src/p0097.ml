(* Project Euler: Problem 97 *)

open Core

let compute n_digits =
  let modulus = Int.pow 10 n_digits in
  Printf.sprintf
    "%0*d"
    n_digits
    (((28433 * Euler.Math.powmod 2 7830457 modulus) + 1) mod modulus)
;;

let solve () = compute 10

(* Test *)

let%test_unit "10" = [%test_eq: string] (compute 10) "8739992577"
