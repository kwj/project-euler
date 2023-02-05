(* Project Euler: Problem 3 *)

open Core

let solve num =
  let base, _ = Euler.Math.factorize num |> List.last_exn in
  base

let exec () =
  Int.to_string (solve 600851475143)

let () = Euler.Task.run exec
