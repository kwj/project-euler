(* Project Euler: Problem 36 *)

open Core

let solve num =
  let is_palindrome = Euler.Math.is_palindrome in
  let rec aux n result =
    if n < 1 then
      result
    else
      if Bool.(is_palindrome n = true) && Bool.(is_palindrome n ~base:2 = true) then
        aux (n - 2) (result + n)
      else
        aux (n - 2) result
  in
  aux num 0

let exec () =
  Int.to_string (solve (1_000_000 - 1))

let () = Euler.Task.run exec
