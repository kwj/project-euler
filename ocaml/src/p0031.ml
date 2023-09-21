(* Project Euler: Problem 31 *)

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

let solve () = compute [ 1; 2; 5; 10; 20; 50; 100; 200 ] 200 |> Int.to_string
;;

(* Test *)

let%test_unit "200" =
  [%test_eq: int] (compute [ 1; 2; 5; 10; 20; 50; 100; 200 ] 200) 73682
