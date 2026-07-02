(* Project Euler: Problem 37 *)

(*
   candidate prime numbers: [2357][1379]*[37] (n >= 10)
*)

open Core

let check_left_truncatable n =
  let rec aux n d =
    if d = 0
    then true
    else if Euler.Math.Prime.is_prime (n mod Int.pow 10 d)
    then aux n (pred d)
    else false
  in
  aux n (Euler.Math.num_of_digits n)
;;

let make_right_truncatable_primes () =
  let next_prime_numbers ns posts =
    List.(
      cartesian_product ns posts
      |> filter_map ~f:(fun (x, y) ->
        let p = (10 * x) + y in
        if Euler.Math.Prime.is_prime p then Some p else None))
  in
  let rec aux res = function
    | [] -> res
    | lst ->
      let cands = next_prime_numbers lst [ 1; 3; 7; 9 ] in
      aux (res @ cands) cands
  in
  next_prime_numbers (aux [ 2; 3; 5; 7 ] [ 2; 3; 5; 7 ]) [ 3; 7 ]
;;

let compute () =
  List.(
    make_right_truncatable_primes ()
    |> filter ~f:check_left_truncatable
    |> reduce_exn ~f:( + ))
;;

let solve () = compute () |> Int.to_string

(* Test *)

let%test_unit "truncatable primes" = [%test_eq: int] (compute ()) 748317
