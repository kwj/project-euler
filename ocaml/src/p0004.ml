(* Project Euler: Problem 4 *)

open Core

let make_segments low high =
  let blk_size = if low = 0 then 1 else low * low in
  List.(
    range (low * low) (high * high) ~stride:blk_size ~stop:`inclusive
    |> map ~f:(fun x -> (x, x + blk_size - 1))
    |> rev)
  |> Sequence.of_list
;;

let find_max_palindrome_number low high (blk_low, blk_high) =
  Sequence.(
    range
      (Euler.Math.isqrt blk_low)
      (if low = 0 then high else min high (blk_high / low))
      ~stop:`inclusive
    |> concat_map ~f:(fun x ->
      range x low ~stride:(-1) ~stop:`inclusive
      |> map ~f:(( * ) x)
      |> drop_while ~f:(( < ) blk_high)
      |> take_while ~f:(( <= ) blk_low)
      |> filter ~f:Euler.Math.is_palindrome)
    |> max_elt ~compare:Int.compare)
;;

let compute n_digits =
  assert (n_digits > 0);
  let low = if n_digits = 1 then 0 else Int.pow 10 (n_digits - 1) in
  let high = Int.pow 10 n_digits - 1 in

  make_segments low high
  |> Sequence.find_map ~f:(find_max_palindrome_number low high)
  |> Option.value_exn
;;

let solve () = compute 3 |> Int.to_string

(* Test *)

let%test_unit "product of two 1-digit numbers" = [%test_eq: int] (compute 1) 9
let%test_unit "product of two 2-digit numbers" = [%test_eq: int] (compute 2) 9009
let%test_unit "product of two 3-digit numbers" = [%test_eq: int] (compute 3) 906609
