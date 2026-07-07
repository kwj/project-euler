(* Project Euler: Problem 55 *)

open Core

let is_rychrel num =
  let cnt = 50 in
  let rev_bignum n = Z.(to_string n |> String.rev |> of_string) in
  let is_palindromic n = Z.equal n (rev_bignum n) in
  Sequence.(
    unfold ~init:(Z.of_int num) ~f:(fun n ->
      match Z.(n + rev_bignum n) with
      | next_n when is_palindromic next_n -> None
      | next_n -> Some (next_n, next_n))
    |> Fun.flip take cnt
    |> length)
  |> ( = ) cnt
;;

let compute upper = Sequence.(range 1 upper ~stop:`inclusive |> count ~f:is_rychrel)
let solve () = compute 10_000 |> Int.to_string

(* Test *)

let%test_unit "10_000" = [%test_eq: int] (compute 10_000) 249
