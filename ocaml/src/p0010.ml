(* Project Euler: Problem 10 *)

open Core

let compute upper =
  Euler.Math.Prime.primes 1 upper |> List.sum (module Int) ~f:Fn.id
;;

let solve () = compute 2_000_000 |> Int.to_string

(* Test *)

let%test_unit "12" = [%test_eq: int] (compute 10) 17
let%test_unit "2_000_000" = [%test_eq: int] (compute 2_000_000) 142913828922
