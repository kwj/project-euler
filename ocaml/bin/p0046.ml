(* Project Euler: Problem 46 *)

(*
  odd composite = prime? + 2 * n^2
 *)

open Core

let is_twice_square num =
  let tmp = Euler.Math.isqrt (num / 2) in
  if tmp * tmp = num / 2 then true else false

let rec find_counter_example n primes =
  if Euler.Math.is_prime n then
    find_counter_example (n + 2) (n :: primes)
  else
    let rec aux = function
      | hd :: _ when Bool.(is_twice_square (n - hd) = true) -> find_counter_example (n + 2) primes
      | _ :: tl -> aux tl
      | [] -> n
    in
    aux primes

let exec () =
  (* '9' is the smallest odd composite number. '[7;5;3]' are odd prime numbers less than '9'. *)
  Int.to_string (find_counter_example 9 [7; 5; 3])

let () = Euler.Task.run exec
