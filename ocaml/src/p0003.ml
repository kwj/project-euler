(* Project Euler: Problem 3 *)

open Core

let compute num =
  let base, _ = Euler.Math.factorize num |> List.last_exn in
  base
;;

let solve () = compute 600851475143 |> Int.to_string

(* Test *)

let%test_unit "13195" = [%test_eq: int] (compute 13195) 29
let%test_unit "600851475143" = [%test_eq: int] (compute 600851475143) 6857
