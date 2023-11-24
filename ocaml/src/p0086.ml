(* Project Euler: Problem 86 *)

(*
 * 1 <= a, b, c <= M
 *
 * we can ignore rotations. there is only one case to consider.
 *   1 <= a <= b <= c <= M
 *    --> 2 <= a + b <= 2c
 *
 *     +--------F
 *     |        |      * sqrt(c^2 + (a+b)^2) must be an integer
 *     |--------|
 *     |        | a+b >= 2
 *     |        |
 *     S--------+
 *          c
 *
 * when a+b <= c <= M
 *   write a+b = x
 *     (a, b) = (1, x-1), (2, x-2), ..., (x-1, 1)
 *   however, because a<=b
 *     num of (a,b) = floor(x/2) = floor((a+b)/2)
 *
 * when a+b > c
 *     num of (a,b) = floor((a+b)/2) - ((a+b-1) - c)
 *
 *     example: c=10, a+b=15
 *       (a,b) = (1,14), ..., (5,10), (6,9), (7,8), ..., (14,1)
 *                            ####################
 *               ^^^^^^^^^^^ = (a+b-1) - c
 *               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ = floor((a+b)/2)
 *
 *     example: c=10, a+b=16
 *       (a,b) = (1,15), ..., (6,10), (7,9), (8,8), ..., (14,1)
 *                            ####################
 *               ^^^^^^^^^^^ = (a+b-1) - c
 *               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ = floor((a+b)/2)
 *)

open Core

let compute thr =
  let rec loop c acc =
    if acc > thr
    then pred c
    else (
      let rec aux ab acc =
        if ab < 2
        then acc
        else (
          let tmp = (c * c) + (ab * ab) in
          if Int.pow (Euler.Math.isqrt tmp) 2 <> tmp
          then aux (pred ab) acc
          else if ab <= c
          then aux (pred ab) (acc + (ab / 2))
          else aux (pred ab) (acc + (ab / 2) - (ab - 1 - c)))
      in
      loop (succ c) (acc + aux (c * 2) 0))
  in
  loop 1 0
;;

let solve () = compute 1_000_000 |> Int.to_string

(* Test *)

let%test_unit "over 2_000" = [%test_eq: int] (compute 1_975) 100
let%test_unit "over 1_000_000" = [%test_eq: int] (compute 1_000_000) 1818
