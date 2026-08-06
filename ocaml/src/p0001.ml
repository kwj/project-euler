(* Project Euler: Problem 1 *)

open Core

let compute limit =
  let f n =
    let tmp = (limit - 1) / n in
    (1 + tmp) * tmp / 2 * n
  in
  f 3 + f 5 - f (Euler.Math.lcm 3 5)
;;

let solve () = compute 1_000 |> Int.to_string

(* Test *)

let%test_unit "limit_10" = [%test_eq: int] (compute 10) 23
let%test_unit "limit_1000" = [%test_eq: int] (compute 1_000) 233168
