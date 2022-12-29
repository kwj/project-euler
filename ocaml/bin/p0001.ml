(* Project Euler: Problem 1 *)

open Core

let make_seq upper =
  List.filter (List.init upper ~f:(fun n -> n)) ~f:(fun n -> n mod 3 = 0 || n mod 5 = 0)

let exec () =
  Int.to_string (List.fold (make_seq 1000) ~init:0 ~f:(+))

let () = Euler.Task.run exec
