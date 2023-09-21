(* Project Euler: Problem 4 *)

open Core

let compute n_digits =
  let lst = ref [] in
  let low = Int.pow 10 (n_digits - 1) in
  let high = Int.pow 10 n_digits - 1 in
  for x = low to high do
    for y = x to high do
      let tmp = x * y in
      if Euler.Math.is_palindrome tmp then lst := tmp :: !lst
    done
  done;
  List.sort ~compare:(fun x y -> y - x) !lst |> List.hd_exn
;;

let solve () = compute 3 |> Int.to_string

(* Test *)

let%test_unit "2-digit numbers" = [%test_eq: int] (compute 2) 9009
let%test_unit "3-digit numbers" = [%test_eq: int] (compute 3) 906609
