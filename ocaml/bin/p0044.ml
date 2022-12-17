(* Project Euler: Problem 44 *)

(*
  P(d) = P(k) - P(j)
    <==>
  d(3d-1) = k(3k-1) - j(3j-1)
          = (k-1)(3(k+1)-1)
    lhs: d(3d-1)
    rhs: (k-j) * (3(k+j)-1) = r1 * r2 [r1=k-j, r2=3(k+j)-1]
      0 < k-j < d
      d mod 3 = r1 mod 3 (because (3d-1) mod 3 = 3(k+j)-1 mod 3 = 2)
      As a result, d >= 4
 *)

open Core

module M = Euler.Math

(*
  get_divisors(n) returns divisors list of n(3n-1) which meet the following requirements:
   - They are less than 'n'.
   - They are congruent to 'n' modulo 3.
 *)
let get_divisors n =
  M.factorize n @ M.factorize (3 * n - 1)    (* 'n' and '3n-1' are relatively prime. *)
  |> M.pfactors_to_divisors
  |> List.filter ~f:(fun i -> i < n && (i mod 3 = n mod 3))

let solve () =
  let check_conditions r1 r2 =
    let pent n = n * (3 * n - 1) / 2 in
    if r2 mod 3 <> 2 then false else
      let tmp = (r2 + 1) / 3 in
      if (r1 + tmp) mod 2 <> 0 then false else
        let k = (r1 + tmp) / 2 in
        let j = k - r1 in
        if M.is_pentagonal ((pent k) + (pent j)) then true else false
  in
  let rec find_minD d =
    let lhs = d * (3 * d - 1) in
    let rec aux = function
      | [] -> None
      | x :: xs -> if check_conditions x (lhs / x) then Some (lhs / 2) else aux xs
    in
    match aux (get_divisors d) with
    | Some n -> n
    | None -> find_minD (succ d)
  in
  find_minD 4

let exec () =
  Int.to_string (solve ())

let () = Euler.Task.run exec
