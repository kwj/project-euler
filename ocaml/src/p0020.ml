(* Project Euler: Problem 20 *)

open Core

let compute num = Z.fac num |> Euler.Util.z_digits |> List.sum (module Int) ~f:Fn.id
let solve () = compute 100 |> Int.to_string

(* Test *)

let%test_unit "10" = [%test_eq: int] (compute 10) 27
let%test_unit "100" = [%test_eq: int] (compute 100) 648
