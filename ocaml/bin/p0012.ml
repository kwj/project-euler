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

let num_of_divs n =
  Euler.Math.divisors n |> List.length

let rec find_num n =
  match n mod 2 with
  | 1 -> if (num_of_divs n) * (num_of_divs ((n + 1) / 2)) > 500 then
           n * (n + 1) / 2
         else
           find_num (succ n)
  | _ -> if (num_of_divs (n / 2)) * (num_of_divs (n + 1)) > 500 then
           n * (n + 1) / 2
         else
           find_num (succ n)

let exec () =
  Int.to_string (find_num 1)

let () = Euler.Task.run exec
