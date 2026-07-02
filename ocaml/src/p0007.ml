(* Project Euler: Problem 7 *)

open Core

let compute n_th =
  assert (n_th > 0);

  Sequence.(
    unfold ~init:2 ~f:(fun n -> Some (n, Euler.Math.Prime.next_prime n))
    |> Fun.flip nth_exn (pred n_th))
;;

let solve () = compute 10_001 |> Int.to_string

(* Test *)

let%test_unit "6" = [%test_eq: int] (compute 6) 13
let%test_unit "10_001" = [%test_eq: int] (compute 10_001) 104743
