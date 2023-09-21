(* Project Euler: Problem 88 *)

(*
  N(k) = a1 + a2 + ... + ak = a1 * a2 * ... * ak
  min_N(k): minimal product-sum N(k)

  when k = 2
    sum {2,2} = prod {2,2}
  when k > 2 and {a1, a2, a3, ..., ak}
    min sum = sum {1, 1, ..., 1} = k
    --> min_N(k) >= k
  when k > 2 and {a1, a2, a3, ..., ak} = {1, ..., 1, 2, k}
    for all k>2, there exists Ak = {a1, a2, ..., ak} = {1, 1, ..., 1, 2, k}, prod Ak = sum Ak = N(k) = 2k
    --> min_N(k) <= 2k

  2 <= k <= 12000
  --> k <= N(k) <= 24000

    >>> math.log2(24000)
    14.550746785383243
    N(2) = {2, 2}
    N(k) = {a1, a2, ..., an, 1, 1, ..., 1}  [k>=3,n<k]
      2 <= n <= 14  [a1, ..., an > 1]
 *)

open Core

let compute limit =
  let tbl = Array.create ~len:(limit + 1) (limit * 2) in
  let rec aux p s length num =
    let k = p - s + length in
    if k <= limit
    then (
      if p < tbl.(k) then tbl.(k) <- p;
      List.range num ((limit * 2) / p) ~stop:`inclusive
      |> List.iter ~f:(fun x -> aux (p * x) (s + x) (succ length) x))
  in
  aux 1 0 0 2;
  Array.to_list tbl
  |> Fn.flip List.drop 2
  |> List.dedup_and_sort ~compare
  |> List.sum (module Int) ~f:Fn.id
;;

let solve () = compute 12_000 |> Int.to_string

(* Test *)

let%test_unit "6" = [%test_eq: int] (compute 6) 30
let%test_unit "12" = [%test_eq: int] (compute 12) 61
let%test_unit "12_000" = [%test_eq: int] (compute 12_000) 7587457
