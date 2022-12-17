(* Project Euler: Problem 35 *)

(*
  When 10 or more, the only numbers that can be used are 1, 3, 7, 9.
  The reason is that rotation number must be prime.
 *)

open Core

let is_prime = Euler.Math.mr_isprime

let is_circular num =
  let k = (String.length (Int.to_string num)) - 1 in
  let d = Int.pow 10 k in
  let rec aux num cnt result =
    if cnt = 0 || Bool.(result = false) then
      result
    else
      let next_num = (num mod 10) * d + (num / 10) in
      aux next_num (pred cnt) (is_prime next_num)
  in
  aux num k true

let solve num =
  let start_num = if num mod 2 = 0 then (pred num) else num in
  let rec aux n result =
    if n < 100 then result
    else
      match n with
      | n when n mod 5 = 0 || n mod 3 = 0
        -> aux (n - 2) result
      | n when Bool.(is_prime n = false)
        -> aux (n - 2) result
      | n when Bool.(is_circular n = true)
        -> aux (n - 2) (succ result)
      | _
        -> aux (n - 2) result
  in
  (aux start_num 0) + 13

let exec () =
  Int.to_string (solve 999_999)

let () = Euler.Task.run exec

