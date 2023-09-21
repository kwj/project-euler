(* Project Euler: Problem 28 *)

(*
   21 22 23 24 25
  20  7  8  9 10
  19  6  1  2 11
  18  5  4  3 12
  17 16 15 14 13
         |  |  |  |
      (n=0, 1, 2, 3, ...)

  the upper right number is:
    1    [n=0]
    (2n+1)**2    [n=>1]

  so, the sum of numbers in the four corners is:
    (2n+1)**2 + ((2n+1)**2 - 2n) + ((2n+1)**2 - 4n) + ((2n+1)**2 - 6n)
      = 16n**2 + 4n + 4   [n>=1]

  Answer: 1 + sum_{n=1}^{(1001-1)/2} (16n**2 + 4n + 4)
*)

open Core

let compute side_len =
  (List.range 1 ((side_len - 1) / 2) ~stop:`inclusive
   |> List.map ~f:(fun n -> (16 * n * n) + (4 * n) + 4)
   |> List.sum (module Int) ~f:Fn.id)
  + 1
;;

let solve () = compute 1_001 |> Int.to_string

(* Test *)

let%test_unit "5" = [%test_eq: int] (compute 5) 101
let%test_unit "1001" = [%test_eq: int] (compute 1_001) 669171001
