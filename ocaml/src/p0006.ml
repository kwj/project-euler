(* Project Euler: Problem 6 *)

open Core

let compute num =
  let pow2 = fun x -> x * x in
  let sq_of_sum = List.(range 1 num ~stop:`inclusive |> reduce_exn ~f:( + ) |> pow2)
  and sum_of_sqs = List.(range 1 num ~stop:`inclusive |> sum (module Int) ~f:pow2) in
  (* The square of sum is equal or larger than the sum of squares. *)
  sq_of_sum - sum_of_sqs
;;

let solve () = compute 100 |> Int.to_string

(* Test *)

let%test_unit "10" = [%test_eq: int] (compute 10) 2640
let%test_unit "100" = [%test_eq: int] (compute 100) 25164150
