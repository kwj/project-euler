(* Project Euler: Problem 63 *)

(*
   n - 1 <= log10(m^n) < n    [m>0, n>0]
   --> n - 1 <= n * log10(m) < n
   --> m < 10
   and
   --> (n - 1)/n <= log10(m)
   --> 1 - 1/n <= log10(m)
   --> 1 - log10(m) <= 1/n
   --> n <= 1/(1 - log10(m))
*)

open Core

let compute () =
  List.range 1 9 ~stop:`inclusive
  |> List.map ~f:(fun m -> 1. /. (1. -. Float.log10 (Int.to_float m)))
  |> List.sum (module Int) ~f:Float.to_int
;;

let solve () = compute () |> Int.to_string

(* Test *)

let%test_unit "Powerful Digit Counts" = [%test_eq: int] (compute ()) 49
