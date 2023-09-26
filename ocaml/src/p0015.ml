(* Project Euler: Problem 15 *)

open Core

(*
 * Method 1: combination
 *   (m+n)! / (m! * n!)
 *
 * Method 2: counting up
 *   [sample: matrix size=3]
 *   1 - 1 - 1 - 1
 *   |   |   |   |
 *   1 - 2 - 3 - 4
 *   |   |   |   |
 *   1 - 3 - 6 - 10
 *   |   |   |   |
 *   1 - 4 - 10 -20(End)
 *)

let compute x y = Euler.Math.binomial (x + y) y
let solve () = compute 20 20 |> Int.to_string

(* Test *)

let%test_unit "(2, 2)" = [%test_eq: int] (compute 2 2) 6
let%test_unit "(20, 20)" = [%test_eq: int] (compute 20 20) 137846528820
