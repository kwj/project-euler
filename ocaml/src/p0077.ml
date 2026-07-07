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
  let rec loop p_lst =
    let len = List.length p_lst in
    let tbl = Array.create ~len:(len + 1) 0 in
    let rec aux = function
      | [] -> tbl.(len)
      | x :: xs ->
        for i = x to len do
          tbl.(i) <- tbl.(i) + tbl.(i - x)
        done;
        aux xs
    in
    tbl.(0) <- 1;
    if aux p_lst > thr then len else loop (plst_gen ())
  in
  loop (plst_gen ())
;;

let solve () = compute 5_000 |> Int.to_string

(* Test *)

let%test_unit "4" = [%test_eq: int] (compute 4) 10
let%test_unit "5_000" = [%test_eq: int] (compute 5_000) 71
