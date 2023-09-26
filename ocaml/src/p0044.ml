(* Project Euler: Problem 44 *)

(*
 * P(d) = P(k) - P(j)
 *   <==>
 * d(3d-1) = k(3k-1) - j(3j-1)
 *         = (k-1)(3(k+1)-1)
 *   lhs: d(3d-1)
 *   rhs: (k-j) * (3(k+j)-1) = r1 * r2 [r1=k-j, r2=3(k+j)-1]
 *     0 < k-j < d
 *     d mod 3 = r1 mod 3 (because (3d-1) mod 3 = 3(k+j)-1 mod 3 = 2)
 *     As a result, d >= 4
 *)

open Core
module M = Euler.Math

(*
 * get_divisors(n) returns divisors of n(3n-1) which meet the following requirements:
 *   - They are less than 'n'.
 *   - They are congruent to 'n' modulo 3.
 * Note: 'n' and '3n-1' are relatively prime.
 *)
let get_divisors n =
  M.factorize n @ M.factorize ((3 * n) - 1)
  |> M.pfactors_to_divisors
  |> List.filter ~f:(fun i -> i < n && i mod 3 = n mod 3)
;;

let check_conditions r1 r2 =
  let pent n = n * ((3 * n) - 1) / 2 in
  if r2 mod 3 <> 2
  then false
  else (
    let tmp = (r2 + 1) / 3 in
    if (r1 + tmp) mod 2 <> 0
    then false
    else (
      let k = (r1 + tmp) / 2 in
      let j = k - r1 in
      if M.is_pentagonal (pent k + pent j) then true else false))
;;

let compute () =
  Sequence.unfold ~init:4 ~f:(fun d -> Some (d, d + 1))
  |> Sequence.find_map ~f:(fun d ->
    let lhs = d * ((3 * d) - 1) in
    if get_divisors d |> List.exists ~f:(fun r1 -> check_conditions r1 (lhs / r1))
    then Some (lhs / 2)
    else None)
  |> Option.value_exn
;;

let solve () = compute () |> Int.to_string

(* Test *)

let%test_unit "Pentagon Numbers" = [%test_eq: int] (compute ()) 5482660
