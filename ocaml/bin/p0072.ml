(* Project Euler: Problem 72 *)

(*
  https://mathproblems123.wordpress.com/2018/05/10/sum-of-the-euler-totient-function/
 *)

open Core

let make_phi_tbl num =
  let limit = num / 2 in
  let phi_tbl = Array.init (limit + 1) ~f:(fun i -> i) in
  for i = 2 to limit do
    if phi_tbl.(i) = i then
      let rec aux j =
        if j <= limit then (
          phi_tbl.(j) <- phi_tbl.(j) - (phi_tbl.(j) / i);
          aux (j + i)
        )
      in
      aux i
  done;
  let _, s_phi_tbl = Array.fold_map phi_tbl ~init:0 ~f:(fun acc elt -> acc + elt, acc + elt) in
  phi_tbl, s_phi_tbl

let sum_1 num tbl =
  let rec aux m acc =
    if m < 2 then
      acc
    else
      aux (pred m) (acc + tbl.(num / m))
  in
  aux (Euler.Math.isqrt num) 0

let sum_2 num tbl =
  let rec aux d acc =
    if d < 1 then
      acc
    else
      if d = num / d then
        aux (pred d) acc
      else
        aux (pred d) (acc + ((num / d - num / (d + 1)) * tbl.(d)))
  in
  aux (Euler.Math.isqrt num) 0

let solve num =
  let _, s_phi_tbl = make_phi_tbl num in
  num * (num + 1) / 2 - (sum_1 num s_phi_tbl) - (sum_2 num s_phi_tbl) - 1

let exec () =
  Int.to_string (solve 1_000_000)

let () = Euler.Task.run exec
