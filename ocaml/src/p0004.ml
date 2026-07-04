(* Project Euler: Problem 4 *)

open Core

let make_segments low high =
  let blk_size = low * low in
  List.(
    range blk_size (high * high) ~stride:blk_size ~stop:`inclusive
    |> map ~f:(fun x -> (x, x + blk_size - 1))
    |> rev)
  |> Sequence.of_list
;;

let find_max_palindrome_number low high (blk_low, blk_high) =
  Sequence.(
    range (Euler.Math.isqrt blk_low) (min high (blk_high / low)) ~stop:`inclusive
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
  let low = Int.pow 10 (n_digits - 1) in
  let high = Int.pow 10 n_digits - 1 in

  make_segments low high
  |> Sequence.find_map ~f:(find_max_palindrome_number low high)
  |> Option.value_exn
;;

let solve () = compute 3 |> Int.to_string

(* Test *)

let%test_unit "2-digit numbers" = [%test_eq: int] (compute 2) 9009
let%test_unit "3-digit numbers" = [%test_eq: int] (compute 3) 906609
