(* Project Euler: Problem 6 *)

open Core

let compute num =
  let sum_of_sq =
    List.range 1 num ~stop:`inclusive |> List.sum (module Int) ~f:(fun x -> x * x)
  in
  let sq_of_sum =
    List.range 1 num ~stop:`inclusive |> List.sum (module Int) ~f:Fn.id |> fun x -> x * x
  in
  abs (sum_of_sq - sq_of_sum)
;;

let solve () = compute 100 |> Int.to_string

(* Test *)

let%test_unit "10" = [%test_eq: int] (compute 10) 2640
let%test_unit "100" = [%test_eq: int] (compute 100) 25164150
