(* Project Euler: Problem 87 *)

(*
  >>> 50000000 ** (1/2)
  7071.067811865475
  >>> 50000000 ** (1/3)
  368.40314986403854
  >>> 50000000 ** (1/4)
  84.08964152537145

  This program is a bit slow. Here is a result on Raspberry Pi 4 Model B.

    % ./_build/default/bin/p0087.exe
    [Problem 87]
    Answer: 1097343
    Elapsed time: 2.94s
 *)

open Core

let compute thr =
  let p_lst = Euler.Math.Prime.primes 1 (Euler.Math.isqrt thr) in
  let res = Hash_set.create (module Int) in

  let x2_lst = List.map p_lst ~f:(fun n -> Int.pow n 2) in

  let y3_limit = Float.(iround_exn ~dir:`Down (of_int thr ** (1. /. 3.))) in
  let y3_lst = List.take_while p_lst ~f:(fun n -> n <= y3_limit)
               |> List.map ~f:(fun n -> Int.pow n 3)
  in
  let z4_limit = Float.(iround_exn ~dir:`Down (of_int thr ** (1. /. 4.))) in
  let z4_lst = List.take_while p_lst ~f:(fun n -> n <= z4_limit)
               |> List.map ~f:(fun n -> Int.pow n 4)
  in
  List.iter z4_lst ~f:(fun z4 ->
    List.iter y3_lst ~f:(fun y3 ->
      List.iter x2_lst ~f:(fun x2 ->
        if z4 + y3 + x2 < thr
        then Hash_set.add res (z4 + y3 + x2))));
  Hash_set.length res
;;

let solve () = compute 50_000_000 |> Int.to_string

(* Test *)

let%test_unit "50" = [%test_eq: int] (compute 50) 4
let%test_unit "50_000_000" = [%test_eq: int] (compute 50_000_000) 1097343

