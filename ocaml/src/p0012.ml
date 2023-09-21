(* Project Euler: Problem 12 *)

(*
   triangle number's formula is n(n + 1)/2 and 'n' and 'n + 1' are coprime.
   Therefore, ...
   - 'n/2' and 'n+1' are coprime (when 'n' is even)
   - 'n' and '(n+1)/2' are coprime (when 'n' is odd)

   assume that f(n) returns number of divisors of 'n'.
   f(a*b) = f(a) * f(b) when 'a' and 'b' are coprime.
*)
open Core

let num_of_divs n = Euler.Math.divisors n |> List.length

let find_num thr =
  let rec aux n =
    match n mod 2 with
    | 1 ->
      if num_of_divs n * num_of_divs ((n + 1) / 2) > thr
      then n * (n + 1) / 2
      else aux (succ n)
    | _ ->
      if num_of_divs (n / 2) * num_of_divs (n + 1) > thr
      then n * (n + 1) / 2
      else aux (succ n)
  in
  aux 1
;;

let compute thr = find_num thr
let solve () = compute 500 |> Int.to_string

(* Test *)

let%test_unit "5" = [%test_eq: int] (compute 5) 28
let%test_unit "500" = [%test_eq: int] (compute 500) 76576500
