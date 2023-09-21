(* Project Euler: Problem 46 *)

(*
   odd composite = prime? + 2 * n^2
*)

open Core

let is_twice_square n = n mod 2 = 0 && Int.pow (Euler.Math.isqrt (n / 2)) 2 = n / 2

let compute () =
  let rec aux odd_primes odd_number =
    if Euler.Math.Prime.is_prime odd_number
    then aux (odd_number :: odd_primes) (odd_number + 2)
    else if List.exists odd_primes ~f:(fun p -> is_twice_square (odd_number - p))
    then aux odd_primes (odd_number + 2)
    else odd_number
  in
  aux [ 31; 29; 23; 19; 17; 13; 11; 7; 5; 3 ] 35
;;

let solve () = compute () |> Int.to_string

(* Test *)

let%test_unit "Goldbach's Other Conjecture" = [%test_eq: int] (compute ()) 5777
