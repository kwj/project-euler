(* Project Euler: Problem 47 *)

open Core

(* List.length (Euler.Math.factorize num) is slow. Therefore, I wrote the following function *)
let num_of_pfactors num =
  let ulimit = Euler.Math.isqrt num in
  let rec div_all n i =
    if n mod i <> 0 || n = 1 then n else div_all (n / i) i
  in
  let rec aux i n result =
    if n <= 1 then
      result
    else
      if i > ulimit then
        succ result
      else
        if n mod i <> 0 then
          aux (succ i) n result
        else
          aux (succ i) (div_all n i) (succ result)
  in
  aux 2 num 0

let rec find_number n cnt =
  if num_of_pfactors n <> 4 then
    find_number (succ n) 0
  else
    if cnt = 3 then
      n - 3
    else
      find_number (succ n) (succ cnt)

let exec () =
  (* 2 * 3 * 5 * 7 = 210 is the smallest number which is product of four prime numbers. *)
  Int.to_string (find_number (2 * 3 * 5 * 7) 0)

let () = Euler.Task.run exec
