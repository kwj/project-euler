(* Project Euler: Problem 21 *)

open Core

let compute limit =
  let spd_tbl = Euler.Math.aliquot_sum_tbl limit in
  List.(
    range 2 limit ~stop:`exclusive
    |> filter_map ~f:(fun x ->
      let y = spd_tbl.(x) in
      if x > y && spd_tbl.(y) = x then Some (x + y) else None)
    |> reduce_exn ~f:( + ))
;;

let solve () = compute 10_000 |> Int.to_string

(* Test *)

let%test_unit "10_000" = [%test_eq: int] (compute 10_000) 31626
