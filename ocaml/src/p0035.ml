(* Project Euler: Problem 35 *)

open Core

let compute limit =
  let module Prime = Euler.Math.Prime in
  let is_circular num =
    let k = Euler.Math.num_of_digits num in
    let d = Int.pow 10 (k - 1) in
    let rec aux n cnt result =
      if cnt = 0 || Bool.(result = false)
      then result
      else (
        let next_n = (n mod 10 * d) + (n / 10) in
        aux next_n (pred cnt) (Prime.is_prime next_n))
    in
    aux num (k - 1) true
  in
  let rec loop count = function
    | [] -> count
    | x :: xs ->
      if Bool.(is_circular x = true) then loop (succ count) xs else loop count xs
  in
  loop 0 (Prime.primes 1 limit)
;;

let solve () = compute 1_000_000 |> Int.to_string

(* Test *)

let%test_unit "100" = [%test_eq: int] (compute 100) 13
let%test_unit "1000000" = [%test_eq: int] (compute 1_000_000) 55
