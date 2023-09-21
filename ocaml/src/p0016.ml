(* Project Euler: Problem 16 *)

open Core

let compute exp = Z.(pow ~$2 exp) |> Euler.Util.z_digits |> List.sum (module Int) ~f:Fn.id
let solve () = compute 1_000 |> Int.to_string

(* Test *)

let%test_unit "15" = [%test_eq: int] (compute 15) 26
let%test_unit "1_000" = [%test_eq: int] (compute 1_000) 1366
