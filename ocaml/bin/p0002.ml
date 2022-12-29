(* Project Euler: Problem 2 *)

open Core

let rec make_fib_seq upper l =
  match l with
  | [] -> make_fib_seq upper [1; 0]
  | m :: n :: _ -> if m + n >= upper then l else make_fib_seq upper (m + n :: l)
  | _ -> l;;

let exec () =
  Int.to_string (List.fold
                   (List.filter (make_fib_seq 4_000_000 []) ~f:(fun n -> n mod 2 = 0))
                   ~f:(+) ~init:0)

let () = Euler.Task.run exec
