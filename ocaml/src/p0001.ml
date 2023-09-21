(* Project Euler: Problem 1 *)

open Core

let sum_of_mults n limit =
    let tmp = limit / n in
    (1 + tmp) * tmp / 2 * n
;;

let compute limit =
  sum_of_mults 3 (limit - 1) + sum_of_mults 5 (limit - 1) - sum_of_mults 15 (limit - 1)
;;

let solve () = compute 1_000 |> Int.to_string

(* Test *)

let%test_unit "limit_10" = [%test_eq: int] (compute 10) 23
let%test_unit "limit_1000" = [%test_eq: int] (compute 1_000) 233168
