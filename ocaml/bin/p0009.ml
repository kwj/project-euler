(* Project Euler: Problem 9 *)

open Core

exception Failure of string

let rec find_triplet a b c =
  if a > 332 then
    raise (Failure "no answer")
  else
    if b > 998 then
      find_triplet (a + 1) (a + 2) (a + 3)
    else
      if c > 999 then
        find_triplet a (b + 1) (b + 2)
      else
        if a + b + c = 1000 && (a * a) + (b * b) = (c * c) then
          (a, b, c)
        else
          find_triplet a b (c + 1)

let exec () =
  let a, b, c = find_triplet 1 2 3 in
  sprintf "%d (a, b, c) = (%d, %d, %d)" (a * b * c) a b c

let () = Euler.Task.run exec
