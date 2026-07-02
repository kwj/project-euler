(* Project Euler: Problem 29 *)

open Core

let make_dupctr_tbl upper =
  let max_exp = Euler.Math.get_max_exp upper ~base:2 in
  let tbl = Array.create 0 ~len:(max_exp + 1) in
  Sequence.(
    range 2 max_exp ~stop:`inclusive
    |> iter ~f:(fun x ->
      let dups = Array.create 0 ~len:(upper + 1) in
      range 1 x ~stop:`exclusive
      |> iter ~f:(fun y ->
        let k = Euler.Math.lcm x y / x in
        range (max k 2) (upper * y / x) ~stop:`inclusive ~stride:k
        |> iter ~f:(fun idx -> dups.(idx) <- 1));
      tbl.(x) <- Array.to_list dups |> List.reduce_exn ~f:( + )));
  tbl
;;

let compute upper =
  let dup_ctr_tbl = make_dupctr_tbl upper in
  let base_limit = Euler.Math.isqrt upper in
  let skip_flag = Array.create false ~len:(base_limit + 1) in
  let ans = ref ((upper - 1) * (upper - 1)) in
  Sequence.(
    range 2 base_limit ~stop:`inclusive
    |> iter ~f:(fun b ->
      if not skip_flag.(b)
      then
        range 0 (Euler.Math.get_max_exp upper ~base:b) ~stop:`inclusive
        |> Fun.flip drop 2
        |> iter ~f:(fun e ->
          ans := !ans - dup_ctr_tbl.(e);
          let tmp = Int.pow b e in
          if tmp <= base_limit then skip_flag.(tmp) <- true)));
  !ans
;;

let solve () = compute 100 |> Int.to_string

(* Test *)

let%test_unit "100" = [%test_eq: int] (compute 100) 9183
