(* Project Euler: Problem 55 *)

open Core

let is_rychrel big_num =
  let rev_bignum n = Z.to_string n |> String.rev |> Z.of_string in
  let rec loop n tmp cnt =
    if cnt = 0
    then true
    else (
      let next_n = Z.(n + tmp) in
      let next_tmp = rev_bignum next_n in
      if Z.equal next_n next_tmp then false else loop next_n next_tmp (pred cnt))
  in
  loop big_num (rev_bignum big_num) 50
;;

let compute upper =
  List.range 1 upper ~stop:`inclusive |> List.count ~f:(fun n -> is_rychrel (Z.of_int n))
;;

let solve () = compute 10_000 |> Int.to_string

(* Test *)

let%test_unit "10_000" = [%test_eq: int] (compute 10_000) 249
