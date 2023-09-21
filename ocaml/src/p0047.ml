(* Project Euler: Problem 47 *)

open Core

let compute nfactors =
  let rec aux x cnt =
    if List.length (Euler.Math.factorize x) <> nfactors
    then aux (succ x) 1
    else if cnt < nfactors
    then aux (succ x) (succ cnt)
    else x - (nfactors - 1)
  in
  (* start from the lowest product of two different primes *)
  aux (2 * 3) 1
;;

let solve () = compute 4 |> Int.to_string

(* Test *)

let%test_unit "2" = [%test_eq: int] (compute 2) 14
let%test_unit "3" = [%test_eq: int] (compute 3) 644
let%test_unit "4" = [%test_eq: int] (compute 4) 134043
