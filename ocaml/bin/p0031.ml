(* Project Euler: Problem 31 *)

open Core

let solve_dp coins target =
  let table = Array.create ~len:(target + 1) 0 in
  let rec aux lst =
    match lst with
    | [] -> table.(target)
    | x :: xs ->
       for i = x to target do
         table.(i) <- table.(i) + table.(i - x)
       done;
       aux xs
  in
  table.(0) <- 1;
  aux coins

let exec () =
  Int.to_string (solve_dp [1; 2; 5; 10; 20; 50; 100; 200] 200)

let () = Euler.Task.run exec
