(* Project Euler: Problem 77 *)

open Core

let plst_generator () =
  let prime = ref 1 in
  let p_lst = ref [] in
  let next () =
    prime := Euler.Math.Prime.next_prime !prime;
    p_lst := !prime :: !p_lst;
    List.rev !p_lst
  in
  next
;;

let compute thr =
  let plst_gen = plst_generator () in
  let rec loop n p_lst =
    let tbl = Array.create ~len:(n + 1) 0 in
    let rec aux lst =
      match lst with
      | [] -> tbl.(n)
      | x :: xs ->
        for i = x to n do
          tbl.(i) <- tbl.(i) + tbl.(i - x)
        done;
        aux xs
    in
    tbl.(0) <- 1;
    if aux p_lst > thr then n else loop (succ n) (plst_gen ())
  in
  loop 1 (plst_gen ())
;;

let solve () = compute 5_000 |> Int.to_string

(* Test *)

let%test_unit "4" = [%test_eq: int] (compute 4) 10
let%test_unit "5_000" = [%test_eq: int] (compute 5_000) 71
