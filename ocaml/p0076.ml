(* Project Euler: Problem 76 *)

(*
  another version of problem 31

  coins: 1, 2, 3, ..., 99
  total: 100
 *)

(* ---------------------------------------------------------------- *)

let solve_dp coins target =
  let table = Array.make (target + 1) 0 in
  let rec aux lst =
    match lst with
    | [] -> table.(target)
    | hd :: tl ->
       for i = hd to target do
         table.(i) <- table.(i) + table.(i - hd)
       done;
       aux tl
  in
  table.(0) <- 1;
  aux coins

let () =
  Printf.printf "Answer: %d\n" (solve_dp (List.init 99 (fun i -> i + 1)) 100)
