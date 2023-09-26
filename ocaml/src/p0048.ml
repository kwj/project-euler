(* Project Euler: Problem 48 *)

(*
 * Here is an information about modular exponentiation.
 *  - https://en.wikipedia.org/wiki/Modular_exponentiation
 *)

open Core

let compute upper =
  let modulus = Int.pow 10 10 in
  List.range 1 upper ~stop:`inclusive
  |> List.filter ~f:(fun n -> n mod 10 <> 0)
  |> List.map ~f:(fun n -> Euler.Math.powmod n n modulus)
  |> List.sum (module Int) ~f:Fn.id
  |> Fun.flip ( % ) modulus
  |> Printf.sprintf "%010d"
;;

let solve () = compute 1_000

(* Test *)

let%test_unit "10" = [%test_eq: string] (compute 10) "0405071317"
let%test_unit "1000" = [%test_eq: string] (compute 1_000) "9110846700"
