(* Project Euler: Problem 2 *)

(*
 * f₁ = 1, f₂ = 2, f₃ = 3, f₄ = 5, f₅ = 8, f₆ = 13, f₇ = 21, f₈ = 34, f₉ = 55, ...
 * assume that k ≥ 7
 *   f(k) = f(k-1) + f(k-2)
 *        = 2f(k-2) + f(k-3)
 *        = 2(f(k-3) + f(k-4)) + f(k-3)
 *        = 3f(k-3) + 2f(k-4)
 *        = 3f(k-3) + 2f(k-5) + 2f(k-6)
 *        = 4f(k-3) - f(k-3) + 2f(k-5) + 2f(k-6)
 *        = 4f(k-3) - (f(k-4) + f(k-5)) + 2f(k-5) + 2f(k-6)
 *        = 4f(k-3) - f(k-4) + f(k-5) + 2f(k-6)
 *        = 4f(k-3) - f(k-4) + (f(k-5) + f(k-6)) + f(k-6)
 *        = 4f(k-3) - f(k-4) + f(k-4) + f(k-6)
 *        = 4f(k-3) + f(k-6)
 *)

open Core

let even_fibs a b =
  Sequence.unfold ~init:(a, b) ~f:(fun (a, b) -> Some (a, (b, (4 * b) + a)))
;;

let compute limit =
  even_fibs 2 8
  |> Sequence.take_while ~f:(fun x -> x <= limit)
  |> Sequence.sum (module Int) ~f:Fn.id
;;

let solve () = compute 4_000_000 |> Int.to_string

(* Test *)

let%test_unit "limit_100" = [%test_eq: int] (compute 100) 44
let%test_unit "limit_4_000_000" = [%test_eq: int] (compute 4_000_000) 4613732
