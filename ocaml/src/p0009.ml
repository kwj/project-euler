(* Project Euler: Problem 9 *)

(*
   a = k(m^2 - n^2), b = k * 2mn, c = k(m^2 + n^2)  [m>n>0, gcd(m,n)=1, m+n is odd]

   abc = k^3 * (m^4 - n^4) * 2mn
   a + b + c = k * 2m(m+n) = 1000

   -> 'k' and 'm' are divisors to 500 (= 1000/2).
   'm+n' is a divisor to 500/m.
   m(m+n) <= 500 --> m <= isqrt(500), m+n <= 500/m
*)

open Core

exception Failure of string

let compute perim =
  let rec loop m =
    if m < 2
    then failwith "no answer"
    else if perim / 2 mod m <> 0
    then loop (pred m)
    else (
      let rec aux x =
        if x >= 2 * m || x > perim / 2 / m
        then loop (pred m)
        else if Euler.Math.gcd m x = 1 && perim / 2 / m mod x = 0
        then (
          let k = perim / 2 / m / x
          and n = x - m in
          Int.pow k 3 * (Int.pow m 4 - Int.pow n 4) * 2 * m * n)
        else aux (x + 2)
      in
      (* assume that x = m + n where x is odd number *)
      aux (if m mod 2 = 1 then m + 2 else m + 1))
  in
  loop (Euler.Math.isqrt (perim / 2))
;;

let solve () = compute 1000 |> Int.to_string

(* Test *)

let%test_unit "12" = [%test_eq: int] (compute (3 + 4 + 5)) 60
let%test_unit "36" = [%test_eq: int] (compute (9 + 12 + 15)) 1620
let%test_unit "1_000" = [%test_eq: int] (compute 1_000) 31875000
