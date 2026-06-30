(* Project Euler: Problem 14 *)

open Core

let get_collatz_length n =
  let rec aux n cnt =
    if n = 1
    then cnt
    else if n mod 2 = 0
    then aux (n / 2) (succ cnt)
    else aux ((3 * n) + 1) (succ cnt)
  in
  aux n 1
;;

let compute thr =
  let max_collatz_number =
    Sequence.(
      range (thr / 2) thr ~stop:`exclusive
      |> map ~f:(fun n -> (n, get_collatz_length n))
      |> max_elt ~compare:(fun (_, x) (_, y) -> Int.compare x y))
  in
  match max_collatz_number with
  | Some (n, _) -> n
  | None -> failwith "no answer"
;;

let solve () = compute 1_000_000 |> Int.to_string

(* Test *)

let%test_unit "1_000_000" = [%test_eq: int] (compute 1_000_000) 837799
