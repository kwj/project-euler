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
 *)

open Core

(*
    0.123456789 | 10111213...979899 | 100101102...997998999  | 100010011002  ...
                       ^[d15]=2
    example (d 15)
      pos = 15 > 1 * 9 * (10 ** 0) = 9
      pos <- 15 - 9 = 6

      pos = 6 <= 2 * 9 * (10 ** 1) = 180
      q <- (6 - 1) / 2 = 2, r <- (6 - 1) % 2 = 1
      num <- 10 ** (2 - 1) + q = 10 + 2 = 12
      d[15] = num / (10 ** (2 - r - 1)) % 10
            = 12 / (10 ** 0) % 10
            = 2
 *)
let d nth =
  let rec loop pos ndigits =
    let block_size = ndigits * 9 * (Int.pow 10 (ndigits - 1)) in
    if pos > block_size then
      loop (pos - block_size) (succ ndigits)
    else
      let q, r = (pos - 1) / ndigits, (pos - 1) mod ndigits in
      let num = (Int.pow 10 (ndigits - 1)) + q in
      (num / (Int.pow 10 (ndigits - r - 1))) mod 10
  in
  loop nth 1

let solve () =
  (d 1) * (d 10) * (d 100) * (d 1_000) * (d 10_000) * (d 100_000) * (d 1_000_000)

let exec () =
  Int.to_string (solve ())

let () = Euler.Task.run exec
