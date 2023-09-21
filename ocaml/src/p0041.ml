(* Project Euler: Problem 41 *)

(*
  (sum_{1}_{9} n) mod 3 = 45 mod 3 = 0  --> 9 digits pandigital number is a multiple of 3, not a prime.
  (sum_{1}_{8} n) mod 3 = 36 mod 3 = 0  --> 8 digits pandigital number is a multiple of 3, not a prime.
  (sum_{1}_{7} n) mod 3 = 28 mod 3 = 1
  (sum_{1}_{6} n) mod 3 = 21 mod 3 = 0  --> 6 digits pandigital number is a multiple of 3, not a prime.
  (sum_{1}_{5} n) mod 3 = 15 mod 3 = 0  --> 5 digits pandigital number is a multiple of 3, not a prime.
  (sum_{1}_{4} n) mod 3 = 10 mod 3 = 1
*)

open Core

let compute () =
  List.find_map [ 7; 4 ] ~f:(fun k ->
    let rec aux = function
      | [] -> None
      | xs :: xss ->
        let n = List.rev xs |> Euler.Util.undigits in
        if Euler.Math.is_pandigital_nz n && Euler.Math.Prime.is_prime n then Some n else aux xss
    in
    aux
      (Euler.Util.permutation
         k
         (List.range k 1 ~stop:`inclusive ~stride:(-1))))
  (* We know that 2413 is a 4-digit pandigital and is also prime. So, the value must exist. *)
  |> Option.value_exn
;;

let solve () = compute () |> Int.to_string

(* Test *)

let%test_unit "Pandigital Prime" = [%test_eq: int] (compute ()) 7652413
