(* Project Euler: Problem 23 *)

open Core

(* make sum of proper divisors table *)
let make_spd_tbl limit =
  let d_tbl = Euler.Math.get_sigma_tbl 1 (limit - 1) in
  Array.iteri ~f:(fun idx _ -> d_tbl.(idx) <- d_tbl.(idx) - idx) d_tbl;
  d_tbl
;;

let compute limit =
  let spd_tbl = make_spd_tbl limit in
  let abndnt_flag = Array.init (Array.length spd_tbl) ~f:(fun n -> n < spd_tbl.(n)) in
  let abndnt_lst = ref [] in
  let rec loop lst acc =
    match lst with
    | [] -> acc
    | x :: xs -> (
      if x mod 2 = 0 && abndnt_flag.(x / 2)
      then abndnt_lst := (x / 2) :: !abndnt_lst;
      if List.exists !abndnt_lst ~f:(fun i -> abndnt_flag.(x - i))
      then loop xs acc
      else loop xs (acc + x))
  in
  loop (List.range 1 limit ~stop:`exclusive) 0
;;

let solve () = compute 28_123 |> Int.to_string

(* Test *)

let%test_unit "28123" = [%test_eq: int] (compute 28_123) 4179871

