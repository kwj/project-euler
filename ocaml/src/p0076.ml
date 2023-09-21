(* Project Euler: Problem 76 *)

(*
  another version of the problem 31

  coins: 1, 2, 3, ..., 99
  total: 100
*)

open Core

let compute coins target =
  let table = Array.create ~len:(target + 1) 0 in
  let rec aux = function
    | [] -> table.(target)
    | x :: xs ->
      for i = x to target do
        table.(i) <- table.(i) + table.(i - x)
      done;
      aux xs
  in
  table.(0) <- 1;
  aux coins
;;

let solve () = compute (List.range 1 99 ~stop:`inclusive) 100 |> Int.to_string
;;

(* Test *)

let%test_unit "5" =
  [%test_eq: int] (compute (List.range 1 4 ~stop:`inclusive) 5) 6

let%test_unit "100" =
  [%test_eq: int] (compute (List.range 1 99 ~stop:`inclusive) 100) 190569291
;;
