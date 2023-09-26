(* Project Euler: Problem 26 *)

open Core
module M = Euler.Math

(* preprocessing *)
let pp n =
  let rec aux divisor dividend =
    if divisor mod dividend = 0 then aux (divisor / dividend) dividend else divisor
  in
  aux (aux n 2) 5
;;

(*
   This function is not correct Carmichael function
   because the function assumes that the argument is not a multiple of 2.
*)
let carmichael n =
  M.factorize n
  |> List.map ~f:(fun (b, e) -> (b - 1) * Int.pow b (e - 1))
  |> List.fold ~init:1 ~f:M.lcm
;;

let find_repetend_length n =
  let num = pp n in
  if num = 1
  then 0
  else List.find_exn (M.divisors (carmichael num)) ~f:(fun k -> M.powmod 10 k num = 1)
;;

let compute upper =
  let rec aux ans max_len = function
    | [] -> ans
    | x :: xs ->
      if x <= max_len
      then ans
      else (
        let cycle = find_repetend_length x in
        if cycle > max_len then aux (pp x) cycle xs else aux ans max_len xs)
  in
  aux 0 0 (List.range upper (upper / 2) ~start:`exclusive ~stop:`inclusive ~stride:(-1))
;;

let solve () = compute 1_000 |> Int.to_string

(* Test *)

let%test_unit "10" = [%test_eq: int] (compute 10) 7
let%test_unit "300" = [%test_eq: int] (compute 300) 289
let%test_unit "1000" = [%test_eq: int] (compute 1_000) 983
