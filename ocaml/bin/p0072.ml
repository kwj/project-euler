(* Project Euler: Problem 72 *)

(*
  https://mathproblems123.wordpress.com/2018/05/10/sum-of-the-euler-totient-function/

  recursive version
 *)

open Core

let rec sumphi num tbl =
  let sum_1 num =
    let rec aux m acc =
      if m < 2 then
        acc
      else
        aux (pred m) (acc + (sumphi (num / m) tbl))
    in
    aux (Euler.Math.isqrt num) 0
  in
  let sum_2 num =
    let rec aux d acc =
      if d < 1 then
        acc
      else
        aux (pred d) (acc + ((num / d - num / (d + 1)) * (sumphi d tbl)))
    in
    aux (num / ((Euler.Math.isqrt num) + 1)) 0
  in
  match Hashtbl.find tbl num with
    Some v -> v
  | None -> let v = num * (num + 1) / 2 - (sum_1 num) - (sum_2 num) in
            Hashtbl.set tbl ~key:num ~data:v;
            v

let solve num =
  let memo_tbl = Hashtbl.create (module Int) in
  (sumphi num memo_tbl) - (sumphi 1 memo_tbl)

let exec () =
  Int.to_string (solve 1_000_000)

let () = Euler.Task.run exec
