(* Project Euler: Problem 50 *)

open Core

let cumsum_generator () =
  let prime = ref 0 in
  let acc = ref 0 in
  let next () =
    prime := Euler.Math.Prime.next_prime !prime;
    acc := !acc + !prime;
    !acc
  in
  next
;;

let init_cumsum_lst cs_gen limit =
  let rec aux lst = if List.hd_exn lst < limit then aux (cs_gen () :: lst) else lst in
  aux [ 0 ]
;;

let compute limit =
  let cs_gen = cumsum_generator () in
  let cs_lst = init_cumsum_lst cs_gen limit in
  let rec aux cs_lst r_idx k =
    let diff = List.(nth_exn cs_lst (r_idx - k) - nth_exn cs_lst r_idx) in
    if diff >= limit
    then aux cs_lst (List.length cs_lst - 1) (pred k)
    else if Euler.Math.Prime.is_prime diff
    then diff
    else (
      match r_idx - k with
      | 0 -> aux (cs_gen () :: cs_lst) r_idx k
      | _ -> aux cs_lst (pred r_idx) k)
  in
  aux cs_lst (List.length cs_lst - 1) (List.length cs_lst - 2)
;;

let solve () = compute 1_000_000 |> Int.to_string

(* Test *)

let%test_unit "10" = [%test_eq: int] (compute 10) 5
let%test_unit "100" = [%test_eq: int] (compute 100) 41
let%test_unit "500" = [%test_eq: int] (compute 500) 499
let%test_unit "1_000" = [%test_eq: int] (compute 1_000) 953
let%test_unit "10_000" = [%test_eq: int] (compute 10_000) 9521
let%test_unit "1_000_000" = [%test_eq: int] (compute 1_000_000) 997651
