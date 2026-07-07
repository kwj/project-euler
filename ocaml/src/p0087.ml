(* Project Euler: Problem 87 *)

open Core

let compute thr =
  let ba = Euler.Bitarray.init thr in
  let p_lst = Euler.Math.Prime.primes 1 (Euler.Math.isqrt thr) in
  let x2_lst = List.map p_lst ~f:(fun n -> n * n) in
  let y3_lst =
    List.(map p_lst ~f:(fun n -> n * n * n) |> take_while ~f:(fun x -> x < thr))
  in
  let z4_lst =
    List.(map x2_lst ~f:(fun n -> n * n) |> take_while ~f:(fun x -> x < thr))
  in
  List.(
    iter z4_lst ~f:(fun z4 ->
      iter y3_lst ~f:(fun y3 ->
        iter x2_lst ~f:(fun x2 ->
          if z4 + y3 + x2 < thr then Euler.Bitarray.set ba (z4 + y3 + x2)))));
  Euler.Bitarray.popcount ba
;;

let solve () = compute 50_000_000 |> Int.to_string

(* Test *)

let%test_unit "50" = [%test_eq: int] (compute 50) 4
let%test_unit "50_000_000" = [%test_eq: int] (compute 50_000_000) 1097343
