(* Project Euler: Problem 10 *)

open Core

let exec () =
  let module E = Euler.Eratosthenes in
  let prime_t = E.generate 2_000_000 in
  Int.to_string (List.fold ~init:0 ~f:(fun acc n -> acc + n) (E.to_list prime_t))

let () = Euler.Task.run exec
