(* Project Euler: Problem 80 *)

(*
  This program needs the Zarith module.

  when n < 100,

    i <= 10^99 * sqrt(n) < i + 1
   -->
    i^2 <= 10^198 * n < (i + 1)^2

  'i' is the 100-digit number we want.
 *)

open Core

let solve num =
  let sum_znum n =
    let open Z in
    let rec aux i acc =
      if Z.equal i Z.zero then
        to_int acc
      else
        aux (i / (of_int 10)) ((i mod (of_int 10)) + acc)
    in
    aux n Z.zero
  in
  let multi_z = Z.(pow ((of_int) 10) 198) in
  let rec loop n result =
    if n > num then
      result
    else
      let sqrt_n = Euler.Math.isqrt n in
      if n = sqrt_n * sqrt_n then
        loop (succ n) result
      else
        loop (succ n) ((sum_znum Z.(sqrt (multi_z * (of_int n)))) + result)
  in
  loop 1 0

let exec () =
  Int.to_string (solve 100)

let () = Euler.Task.run exec
