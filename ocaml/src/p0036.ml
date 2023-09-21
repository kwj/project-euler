(* Project Euler: Problem 36 *)

open Core

let compute limit =
  let is_palindrome = Euler.Math.is_palindrome in

  (* Using the List module is slow in this problem. *)
  Sequence.range 1 limit ~stop:`exclusive ~stride:2
  |> Sequence.filter ~f:(fun n -> is_palindrome n ~base:10 && is_palindrome n ~base:2)
  |> Sequence.sum (module Int) ~f:Fn.id
;;

let solve () = compute 1_000_000 |> Int.to_string

(* Test *)

let%test_unit "1000000" = [%test_eq: int] (compute 1_000_000) 872187
