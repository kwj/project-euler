(* Project Euler: Problem 14 *)

open Core

let find_max_seq ulimit =
  let cache = Array.create ~len:(ulimit + 1) 0 in
  let number = ref 0 in
  let max_len = ref 0 in
  let rec aux term start steps =
    if term = 1 || term < start then
      term, steps
    else
      match term mod 2 with
      | 0 -> aux (term / 2) start (succ steps)
      | _ -> aux (3 * term + 1) start (succ steps)
  in
  for i = 1 to ulimit do
    let term, steps = aux i i 0 in
    cache.(i) <- steps + cache.(term);
    if cache.(i) > !max_len then (
      max_len := cache.(i);
      number := i
    )
  done;
  !number, !max_len + 1

let exec () =
  let num, len = find_max_seq 999_999 in
  sprintf "%d (%d terrms)" num len

let () = Euler.Task.run exec
