(* Project Euler: Problem 6 *)

open Core

let exec () =
  let numbers = List.init 100 ~f:(fun n -> n + 1) in
  let sum_list lst = List.fold_left lst ~init:0 ~f:(+) in
  Int.to_string (abs (sum_list (List.map numbers ~f:(fun n -> n * n)) - (Int.pow (sum_list numbers) 2)))

let () = Euler.Task.run exec
