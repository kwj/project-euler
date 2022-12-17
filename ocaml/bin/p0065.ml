(* Project Euler: Problem 65 *)

(*
  e = [2; 1, 2, 1, 1, 4, 1, 1, 6, 1, ...]
      [a{0}; a{1}, a{2}, ...]

    i  a{i-1}  n(numerator)  d(denominator)
   ----------------------------------------
    1   2         2             1
    2   1         3             1
    3   2         8             3
    4   1        11             4
    5   1        19             7
    6   4        87            32
    7   1       106            39
    8   1       193            71
    9   6      1264           465
   10   1      1457           536
             ...
    i c(i)     n(i)          d(i)

    when i > 2:
      n(i) = n(i-1)*c(i) + n(i-2), n(2) = 3, n(1) = 2
      d(i) = d(i-1)*c(i) + d(i-2), d(2) = 1, d(1) = 1

      c(i) = | 1    (i mod 3 <> 0)
             | 2n/3 (i mod 3 = 0)   
 *)

open Core

let sum_of_digits num =
  let rec aux n result =
    if Z.(equal n zero) then
      result
    else
      aux Z.(div n ~$10) (result + Z.(to_int (rem n ~$10)))
  in
  aux num 0

let c = function
  | n when n mod 3 = 0 -> Z.of_int (2 * n / 3)
  | _ -> Z.one

let solve start upper =
  let rec loop i n1 n2 =
    if i > upper then
      sum_of_digits n1
    else
      loop (succ i) Z.(n1 * (c i) + n2) n1
  in
  loop start Z.(~$3) Z.(~$2)

let exec () =
  Int.to_string (solve 3 100)

let () = Euler.Task.run exec
