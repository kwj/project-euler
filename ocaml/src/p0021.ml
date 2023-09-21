(* Project Euler: Problem 21 *)

open Core

(* make sum of proper divisors table *)
let make_spd_tbl limit =
  let d_tbl = Euler.Math.get_sigma_tbl 1 (limit - 1) in
  Array.iteri ~f:(fun idx _ -> d_tbl.(idx) <- d_tbl.(idx) - idx) d_tbl;
  d_tbl
;;

let compute limit =
  let spd_tbl = make_spd_tbl limit in
  List.range 2 limit ~stop:`exclusive
  |> List.filter_map ~f:(fun x ->
    if x > spd_tbl.(x) && spd_tbl.(spd_tbl.(x)) = x then Some (x + spd_tbl.(x)) else None)
  |> List.sum (module Int) ~f:Fn.id
;;

let solve () = compute 10_000 |> Int.to_string

(* Test *)

let%test_unit "10_000" = [%test_eq: int] (compute 10_000) 31626
