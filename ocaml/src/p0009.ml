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

let compute perim =
  let half_perim = perim / 2 in
  let m, n =
    Sequence.(
      range 2 (Euler.Math.isqrt half_perim) ~stop:`inclusive
      |> filter ~f:(fun m -> half_perim mod m = 0)
      |> concat_map ~f:(fun m ->
        range
          ((m mod 2) + 1)
          (min (m - 1) ((half_perim / m) - m))
          ~stride:2
          ~stop:`inclusive
        |> map ~f:(fun n -> (m, n)))
      |> filter ~f:(fun (m, n) -> Euler.Math.gcd m n = 1 && half_perim mod (m + n) = 0)
      (* The problem statement assumes that there is exactly only one Pythagorean triplet *)
      |> hd_exn)
  in
  let k = half_perim / m / (m + n) in
  Int.(pow k 3 * (pow m 4 - pow n 4) * 2 * m * n)
;;

let solve () = compute 1000 |> Int.to_string

(* Test *)

let%test_unit "12" = [%test_eq: int] (compute (3 + 4 + 5)) 60
let%test_unit "36" = [%test_eq: int] (compute (9 + 12 + 15)) 1620
let%test_unit "1_000" = [%test_eq: int] (compute 1_000) 31875000
