(* Project Euler: Problem 2 *)

(*
  f₁ = 1, f₂ = 2, f₃ = 3, f₄ = 5, f₅ = 8, f₆ = 13, f₇ = 21, f₈ = 34, f₉ = 55, ...

  assume that k ≥ 7
    f(k) = f(k-1) + f(k-2)
         = 2f(k-2) + f(k-3)
         = 2(f(k-3) + f(k-4)) + f(k-3)
         = 3f(k-3) + 2f(k-4)
         = 3f(k-3) + 2f(k-5) + 2f(k-6)
         = 4f(k-3) - f(k-3) + 2f(k-5) + 2f(k-6)
         = 4f(k-3) - (f(k-4) + f(k-5)) + 2f(k-5) + 2f(k-6)
         = 4f(k-3) - f(k-4) + f(k-5) + 2f(k-6)
         = 4f(k-3) - f(k-4) + (f(k-5) + f(k-6)) + f(k-6)
         = 4f(k-3) - f(k-4) + f(k-4) + f(k-6)
         = 4f(k-3) + f(k-6)
*)

open Core

let rec make_even_fib_seq upper l =
  match l with
  | [] -> make_even_fib_seq upper [8; 2]
  | m :: n :: _ -> if m + n >= upper then l else make_even_fib_seq upper (4 * m + n :: l)
  | _ -> l;;

let exec () =
  Int.to_string (List.fold (make_even_fib_seq 4_000_000 []) ~f:(+) ~init:0)

let () = Euler.Task.run exec
