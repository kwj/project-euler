(* Project Euler: Problem 37 *)

(*
   candidate prime numbers: [2357][1379]*[37] (n >= 10)
*)

open Core

let is_prime = Euler.Math.Prime.mr_isprime

let check_left_truncatable n =
  let rec aux n d =
    if d = 0
    then true
    else if is_prime (n mod (Int.pow 10 d))
    then aux n (pred d)
    else false
  in
  aux n (Euler.Math.num_of_digits n)
;;

let make_right_truncatable_primes () =
  let module S = Sequence in
  let rec aux res = function
    | [] -> res
    | lst ->
      let cands =
        S.cartesian_product (S.of_list lst) (S.of_list [ 1; 3; 7; 9])
        |> S.map ~f:(fun (x, y) -> 10 * x + y)
        |> S.filter ~f:(fun n -> is_prime n)
        |> S.to_list
      in
      aux (res @ cands) cands
  in
  S.cartesian_product (S.of_list (aux [ 2; 3; 5; 7 ] [ 2; 3; 5; 7 ])) (S.of_list [ 3; 7 ])
  |> S.map ~f:(fun (x, y) -> 10 * x + y)
  |> S.filter ~f:(fun n -> is_prime n)
  |> S.to_list
;;

let compute () =
  List.filter ~f:(fun n -> check_left_truncatable n) (make_right_truncatable_primes ())
  |> List.sum (module Int) ~f:Fn.id
;;

let solve () = compute () |> Int.to_string

(* Test *)

let%test_unit "truncatable primes" = [%test_eq: int] (compute ()) 748317
