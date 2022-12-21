(* Project Euler: Problem 37 *)

(*
  candidate numbers: [2357][1379]*[37] (n >= 10)
                           ----------- base_plst
 *)

open Core

let is_prime = Euler.Math.mr_isprime

let add_prefix_num pre_lst post_lst =
  let ndigits = Euler.Math.num_of_digits in
  let rec aux lst result =
    match lst with
    | [] -> result
    | x :: xs -> aux xs (result @ List.map post_lst ~f:(fun n -> x * (Int.pow 10 (ndigits n)) + n))
  in
  aux pre_lst []

let next_base_plst base_plst =
  List.filter (add_prefix_num [1; 3; 7; 9] base_plst) ~f:is_prime

let pickup_primes base_plst =
  let rec is_truncable_prime num =
    if num = 0 then
      true
    else
      if is_prime num then
        is_truncable_prime (num / 10)
      else
        false
  in
  List.filter (add_prefix_num [2; 3; 5; 7] base_plst) ~f:is_truncable_prime

let solve () =
  let rec aux base_plst acc =
    if List.length acc >= 11 then
      (* the problem statement says there are only 11 of them. *)
      acc
    else
      aux (next_base_plst base_plst) (pickup_primes base_plst @ acc)
  in
  aux [3; 7] []

let exec () =
  Int.to_string (List.fold ~init:0 ~f:(+) (solve ()))

let () = Euler.Task.run exec
