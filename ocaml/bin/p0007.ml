(* Project Euler: Problem 7 *)

open Core

let nth_prime num =
  let is_prime = Euler.Math.is_prime in
  let rec aux i n =
    if Bool.(is_prime i = true) then
      if n = 1 then i else aux (i + 2) (n - 1)
    else
      aux (i + 2) n
  in
  aux 3 num    (* skip '2' *)

let exec () =
  Int.to_string (nth_prime 10_000)  (* skip '2, so use 10000 instead of 10001 *)

let () = Euler.Task.run exec
