(* Project Euler: Problem 5 *)

open Core

let compute num = List.range 1 num ~stop:`inclusive |> List.fold ~init:1 ~f:Euler.Math.lcm
let solve () = compute 20 |> Int.to_string

(* Test *)

let%test_unit "10" = [%test_eq: int] (compute 10) 2520
let%test_unit "20" = [%test_eq: int] (compute 20) 232792560
