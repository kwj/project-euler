(* Project Euler: Problem 73 *)

open Core

let solve limit =
  let rec dfs left right =
    let x = left + right in
    if x > limit then
      0
    else
      (dfs left x) + (dfs x right) + 1
  in
  dfs 3 2

let exec () =
  Int.to_string (solve 12_000)

let () = Euler.Task.run exec
