(* Project Euler: Problem 5 *)

open Core

let lcm_list lst =
  List.fold_left lst ~f:Euler.Math.lcm ~init:1

let exec () =
  Int.to_string (lcm_list (List.init 20 ~f:(fun n -> n + 1)))

let () = Euler.Task.run exec
