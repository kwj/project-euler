(* Project Euler: Problem 23 *)

open Core

let compute limit =
  let spd_tbl = Euler.Math.aliquot_sum_tbl limit in
  let abndnt_flag = Array.init (Array.length spd_tbl) ~f:(fun n -> n < spd_tbl.(n)) in
  let abndnt_lst = ref [] in
  let rec loop lst acc =
    match lst with
    | [] -> acc
    | x :: xs ->
      if x mod 2 = 0 && abndnt_flag.(x / 2) then abndnt_lst := (x / 2) :: !abndnt_lst;
      if List.exists !abndnt_lst ~f:(fun i -> abndnt_flag.(x - i))
      then loop xs acc
      else loop xs (acc + x)
  in
  loop (List.range 1 limit ~stop:`exclusive) 0
;;

let solve () = compute 28_123 |> Int.to_string

(* Test *)

let%test_unit "28123" = [%test_eq: int] (compute 28_123) 4179871
