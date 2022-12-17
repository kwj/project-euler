(* Project Euler: Problem 92 *)

(*
  9^2 = 81
  -->
        99 -> 9^2 * 2 = 162
       999 -> 9^2 * 3 = 243
      9999 -> 9^2 * 4 = 324
     99999 -> 9^2 * 5 = 405
    999999 -> 9^2 * 6 = 486
   9999999 -> 9^2 * 7 = 567
 *)

open Core

let next_num num =
  let rec aux n acc =
    if n = 0 then
      acc
    else
      let x = n mod 10 in
      aux (n / 10) (acc + (x * x))
  in
  aux num 0

let create_tbl limit =
  let tbl = Array.create ~len:limit 0 in
  tbl.(1) <- 1; tbl.(89) <- 89;
  let chain num =
    let rec loop n lst =
      if tbl.(n) <> 0 then
        tbl.(n), lst
      else 
        loop (next_num n) (n :: lst)
    in
    loop num []
  in
  for i = 2 to limit - 1 do
    if tbl.(i) = 0 then (
      let n, lst = chain i in
      List.iter lst ~f:(fun i -> tbl.(i) <- n)
    )
  done;
  tbl

let solve limit =
  let tbl = create_tbl limit in
  Array.fold ~init:0 ~f:(fun acc n -> if n = 89 then succ acc else acc) tbl

let exec () =
  Int.to_string (solve 10_000_000)

let () = Euler.Task.run exec
