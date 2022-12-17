(* Project Euler: Problem 29 *)

open Core

let solve () =
  let rec loop_a a acc =
    let rec loop_b b acc =
      if b < 2 then
        acc
      else
        loop_b (pred b) ((Z.pow (Z.of_int a) b) :: acc)
    in
    if a < 2 then
      acc
    else
      loop_a (pred a) (loop_b 100 acc)
  in
  List.dedup_and_sort (loop_a 100 []) ~compare:Z.compare |> List.length

let exec () =
  Int.to_string (solve ())

let () = Euler.Task.run exec
