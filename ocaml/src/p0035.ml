(* Project Euler: Problem 35 *)

open Core

let compute limit =
  let is_circular_prime prime =
    let k = Euler.Math.num_of_digits prime in
    let d = Int.pow 10 (k - 1) in
    Sequence.(
      unfold ~init:prime ~f:(fun n -> Some (n, (n mod 10 * d) + (n / 10)))
      |> Fun.flip drop 1
      |> Fun.flip take (k - 1)
      |> for_all ~f:Euler.Math.Prime.is_prime)
  in
  Euler.Math.Prime.primes 1 limit |> List.count ~f:is_circular_prime
;;

let solve () = compute 1_000_000 |> Int.to_string

(* Test *)

let%test_unit "100" = [%test_eq: int] (compute 100) 13
let%test_unit "1000000" = [%test_eq: int] (compute 1_000_000) 55
