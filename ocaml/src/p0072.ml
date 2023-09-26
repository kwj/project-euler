(* Project Euler: Problem 72 *)

(*
   https://mathproblems123.wordpress.com/2018/05/10/sum-of-the-euler-totient-function/

   recursive version
*)

open Core

let rec sumphi n tbl =
  let sum_1 n =
    let rec aux m acc = if m < 2 then acc else aux (pred m) (acc + sumphi (n / m) tbl) in
    aux (Euler.Math.isqrt n) 0
  in
  let sum_2 n =
    let rec aux d acc =
      if d < 1
      then acc
      else aux (pred d) (acc + (((n / d) - (n / (d + 1))) * sumphi d tbl))
    in
    aux (n / (Euler.Math.isqrt n + 1)) 0
  in
  match Hashtbl.find tbl n with
  | Some v -> v
  | None ->
    let v = (n * (n + 1) / 2) - sum_1 n - sum_2 n in
    Hashtbl.set tbl ~key:n ~data:v;
    v
;;

let compute n =
  let memo_tbl = Hashtbl.create (module Int) in
  sumphi n memo_tbl - sumphi 1 memo_tbl
;;

let solve () = compute 1_000_000 |> Int.to_string

(* Test *)

let%test_unit "8" = [%test_eq: int] (compute 8) 21
let%test_unit "1_000_000" = [%test_eq: int] (compute 1_000_000) 303963552391
