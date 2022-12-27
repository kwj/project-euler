(* Project Euler: Problem 71 *)

(*
  Farey sequence

  2/5, 3/7
    -> 2/5, (2+3)/(5+7), 3/7
    -> 2/5, (2+3)/(5+7), (2+3+3)/(5+7+7), 3/7
    -> 2/5, (2+3)/(5+7), (2+3+3)/(5+7+7), (2+3+3+3)/(5+7+7+7), 3/7
     ...
    -> 2/5, ..., (2+3x)/(5+7x), 3/7

      5+7x <= 1_000_000
 *)

open Core

let solve num =
  let x = (num - 5) / 7 in
  2 + 3 * x

let exec () =
  Int.to_string (solve 1_000_000)

let () = Euler.Task.run exec
