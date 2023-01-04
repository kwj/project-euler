(* Project Euler: Problem 74 *)

open Core

let fact_sum num =
  let fact = [|1; 1; 2; 6; 24; 120; 720; 5040; 40320; 362880|] in
  let rec loop n result =
    if n = 0 then
      result
    else
      loop (n / 10) (result + fact.(n mod 10))
  in
  if num = 0 then fact.(0) else loop num 0

let update_chain_info c_info idx c_lst =
  let limit = Array.length c_info in
  let rec aux len lst =
    match lst with
    | x :: xs -> if x >= limit then
                   aux (succ len) xs
                 else
                   c_info.(x) <- len;
                   aux (succ len) xs
    | [] -> ()
  in
  aux (c_info.(idx) + 1) c_lst;
  if c_info.(idx) < 60 && (c_info.(idx) + List.length c_lst) >= 60 then 1 else 0

let solve limit =
  let chain_info = Array.create ~len:(limit + 1) 0 in
  chain_info.(145) <- 1;
  chain_info.(169) <- 3; chain_info.(363601) <- 3; chain_info.(1454) <- 3;
  chain_info.(871) <- 2; chain_info.(45361) <- 2;
  chain_info.(872) <- 2; chain_info.(45362) <- 2;

  let rec aux idx lst =
    let next_idx = fact_sum idx in
    if idx = next_idx then (
      chain_info.(idx) <- 1;
      update_chain_info chain_info idx (List.tl_exn lst)
    ) else (
      if next_idx <= limit && chain_info.(next_idx) <> 0 then
        update_chain_info chain_info next_idx lst
      else
        aux (next_idx) (next_idx :: lst)
    )
  in

  let rec loop num result =
    if num < 1 then
      result
    else
      loop (pred num) (result + (aux num [num]))
  in
  loop limit 0

let exec () =
  Int.to_string (solve 999_999)

let () = Euler.Task.run exec
