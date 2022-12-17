(* Project Euler: Problem 76 *)

(*
  another version of problem 31

  coins: 1, 2, 3, ..., 99
  total: 100
 *)

open Core

let solve coins target =
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
  Int.to_string (solve (List.init 99 ~f:(fun i -> i + 1)) 100)

let () = Euler.Task.run exec
