(* Project Euler: Problem 29 *)

open Core

module S = Sequence

let make_dupctr_tbl upper =
  let max_exp = Euler.Math.get_max_exp upper ~base:2 in
  let dup_ctr = Array.create 0 ~len:(max_exp + 1) in
  S.range 2 max_exp ~stop:`inclusive |> S.iter ~f:(fun x ->
    let dups = Array.create 0 ~len:(upper + 1) in
    S.range 1 x ~stop:`exclusive |> S.iter ~f:(fun y ->
      let k = (Euler.Math.lcm x y) / x in
      S.range (max k 2) (upper * y / x) ~stop:`inclusive ~stride:k
      |> S.iter ~f:(fun idx -> dups.(idx) <- 1));
    dup_ctr.(x) <- (Array.to_list dups |> List.sum (module Int) ~f:Fn.id));
  dup_ctr
;;

let compute upper =
  let dup_ctr = make_dupctr_tbl upper in
  let base_limit = Euler.Math.isqrt upper in
  let skip_flag = Array.create false ~len:(base_limit + 1) in
  let ans = ref ((upper - 1) * (upper - 1)) in
  S.range 2 base_limit ~stop:`inclusive |> S.iter ~f:(fun b ->
    if Bool.(skip_flag.(b) = false)
    then
      S.range 0 (Euler.Math.get_max_exp upper ~base:b) ~stop:`inclusive
      |> Fun.flip S.drop 2
      |> S.iter ~f:(fun e ->
        ans := !ans - dup_ctr.(e);
        let tmp = Int.pow b e in
        if tmp <= base_limit then skip_flag.(tmp) <- true));
  !ans
;;

let solve () = compute 100 |> Int.to_string

(* Test *)

let%test_unit "100" = [%test_eq: int] (compute 100) 9183
