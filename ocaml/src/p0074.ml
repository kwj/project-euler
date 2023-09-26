(* Project Euler: Problem 74 *)

open Core

let fact_sum num =
  let fact = [| 1; 1; 2; 6; 24; 120; 720; 5040; 40320; 362880 |] in
  let rec loop n result =
    if n = 0 then result else loop (n / 10) (result + fact.(n mod 10))
  in
  if num = 0 then fact.(0) else loop num 0
;;

let update_chain_tbl tbl idx lst =
  let tbl_size = Array.length tbl in
  let rec aux len lst =
    match lst with
    | [] -> ()
    | x :: xs ->
      if x >= tbl_size then aux (succ len) xs else tbl.(x) <- len;
      aux (succ len) xs
  in
  aux (tbl.(idx) + 1) lst;
  if tbl.(idx) < 60 && tbl.(idx) + List.length lst >= 60 then 1 else 0
;;

let compute limit =
  let limit = limit - 1 in
  let chain_tbl = Array.create ~len:(limit + 1) 0 in
  chain_tbl.(145) <- 1;
  chain_tbl.(169) <- 3;
  chain_tbl.(363601) <- 3;
  chain_tbl.(1454) <- 3;
  chain_tbl.(871) <- 2;
  chain_tbl.(45361) <- 2;
  chain_tbl.(872) <- 2;
  chain_tbl.(45362) <- 2;

  let rec aux idx lst =
    let next_idx = fact_sum idx in
    if idx = next_idx
    then (
      chain_tbl.(idx) <- 1;
      update_chain_tbl chain_tbl idx (List.tl_exn lst))
    else if next_idx <= limit && chain_tbl.(next_idx) <> 0
    then update_chain_tbl chain_tbl next_idx lst
    else aux next_idx (next_idx :: lst)
  in

  Sequence.range limit 1 ~stop:`inclusive ~stride:(-1)
  |> Sequence.map ~f:(fun n -> aux n [ n ])
  |> Sequence.sum (module Int) ~f:Fn.id
;;

let solve () = compute 1_000_000 |> Int.to_string

(* Test *)

let%test_unit "Digit Factorial Chains" = [%test_eq: int] (compute 1_000_000) 402
