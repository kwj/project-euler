(* Project Euler: Problem 40 *)

(*
    0.123456789 | 10111213...979899 | 100101102...997998999  | 100010011002  ...
      ---------   -----------------   ---------------------   -----------------
  len: 1 * 9       2 * 90              3 * 900                 4 * 9000      ...
       1 * 9 * 1   2 * 9 * 10          3 * 9 * 100             4 * 9 * 1000  ...
         --> block_num * 9 * base

  block #1: 1-digit number
  block #2: 2-digits number
  block #3: 3-digits number
    ...
  block #n: n-digits number

  Example: d 15

    0.123456789 | 10111213...979899 | 100101102...997998999  | 100010011002  ...
                       ^[d15]=2
      pos = 15 > 1 * 9 * (10 ** 0) = 9
      pos <- 15 - 9 = 6

      pos = 6 <= 2 * 9 * (10 ** 1) = 180
      q <- (6 - 1) / 2 = 2, r <- (6 - 1) % 2 = 1
      num <- 10 ** (2 - 1) + q = 10 + 2 = 12
      d[15] = num / (10 ** (2 - r - 1)) % 10
            = 12 / (10 ** 0) % 10
            = 2
*)

open Core

let d nth =
  let rec loop pos n_digits =
    let block_size = n_digits * 9 * Int.pow 10 (n_digits - 1) in
    if pos > block_size
    then loop (pos - block_size) (succ n_digits)
    else (
      let q, r = ((pos - 1) / n_digits, (pos - 1) mod n_digits) in
      let num = Int.pow 10 (n_digits - 1) + q in
      num / Int.pow 10 (n_digits - r - 1) mod 10)
  in
  loop nth 1
;;

let compute () = d 1 * d 10 * d 100 * d 1_000 * d 10_000 * d 100_000 * d 1_000_000
let solve () = compute () |> Int.to_string

(* Test *)

let%test_unit "Champernowne's Constant" = [%test_eq: int] (compute ()) 210
