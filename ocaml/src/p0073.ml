(* Project Euler: Problem 73 *)

open Core

let make_mobius_tbl limit =
  let p_tbl = Array.init (limit + 1) ~f:Fn.id in
  let mu_tbl = Array.create ~len:(limit + 1) 0 in

  for i = 2 to Euler.Math.isqrt limit do
    if p_tbl.(i) = i
    then (
      let k = i * i in
      List.range k limit ~stop:`inclusive ~stride:i
      |> List.iter ~f:(fun j -> p_tbl.(j) <- i);
      List.range k limit ~stop:`inclusive ~stride:k
      |> List.iter ~f:(fun j -> p_tbl.(j) <- 0))
  done;

  mu_tbl.(1) <- 1;
  for i = 2 to limit do
    if p_tbl.(i) <> 0 then mu_tbl.(i) <- -mu_tbl.(i / p_tbl.(i))
  done;

  mu_tbl
;;

let f x =
  List.range 1 x ~stop:`inclusive
  |> List.map ~f:(fun j -> ((j - 1) / 2) - (j / 3))
  |> List.sum (module Int) ~f:Fn.id
;;

let g limit =
  let mu_tbl = make_mobius_tbl limit in
  List.range 1 limit ~stop:`inclusive
  |> List.map ~f:(fun k -> mu_tbl.(k) * f (limit / k))
  |> List.sum (module Int) ~f:Fn.id
;;

let compute limit = g limit
let solve () = compute 12_000 |> Int.to_string

(* Test *)

let%test_unit "8" = [%test_eq: int] (compute 8) 3
let%test_unit "12_000" = [%test_eq: int] (compute 12_000) 7295372
