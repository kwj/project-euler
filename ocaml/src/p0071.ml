(* Project Euler: Problem 71 *)

(*
 * Farey sequence
 *
 * 2/5, 3/7
 *   -> 2/5, (2+3)/(5+7), 3/7
 *   -> 2/5, (2+3)/(5+7), (2+3+3)/(5+7+7), 3/7
 *   -> 2/5, (2+3)/(5+7), (2+3+3)/(5+7+7), (2+3+3+3)/(5+7+7+7), 3/7
 *    ...
 *   -> 2/5, ..., (2+3x)/(5+7x), 3/7
 *
 *     5+7x <= 1_000_000
 *)

open Core

let compute n =
  let x = (n - 5) / 7 in
  2 + (3 * x)
;;

let solve () = compute 1_000_000 |> Int.to_string

(* Test *)

let%test_unit "8" = [%test_eq: int] (compute 8) 2
let%test_unit "1_000_000" = [%test_eq: int] (compute 1_000_000) 428570
