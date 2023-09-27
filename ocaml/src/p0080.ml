(* Project Euler: Problem 80 *)

(*
 * This program needs the Zarith module.
 *
 * when n < 100,
 *
 *   i <= 10^99 * sqrt(n) < i + 1
 *  -->
 *   i^2 <= 10^198 * n < (i + 1)^2
 *
 * 'i' is the 100-digit number we want.
 *)

open Core

let compute upper n_digit =
  let const_pow_of_10 = Z.pow (Z.of_int 10) ((n_digit - 1) * 2) in
  List.range 1 upper ~stop:`inclusive
  |> List.map ~f:(fun n ->
    if Int.pow (Euler.Math.isqrt n) 2 = n
    then 0
    else
      Z.(sqrt (const_pow_of_10 * ~$n))
      |> Euler.Util.z_digits
      |> List.sum (module Int) ~f:Fn.id)
  |> List.sum (module Int) ~f:Fn.id
;;

let solve () = compute 100 100 |> Int.to_string

(* Test *)

let%test_unit "100" = [%test_eq: int] (compute 100 100) 40886
