(* Project Euler: Problem 12 *)

open Core

let rec find_num a b =
  let tri_num = a + b in
  let divs = Euler.Math.divisors tri_num in
  if List.length divs >= 500 then
    tri_num
  else
    find_num tri_num (succ b)

let exec () =
  Int.to_string (find_num 0 1)

let () = Euler.Task.run exec
