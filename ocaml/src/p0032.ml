(* Project Euler: Problem 32 *)

(*
   m * n = mn (multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital)

   - numbers of digits of multiplicand/multiplier must be 4 or less.
   - if number of digits of multiplicand is 4, number of digits of multiplier is 1.
   - if number of digits of multiplicand is 3, number of digits of multiplier is 2.

   multiplicand/multiplier/product : 4-digits/1-digit/4-digits or 3-digits/2-digits/4-digits
*)

open Core

let compute () =
  let module U = Euler.Util in
  Sequence.append
    (Sequence.cartesian_product
       (Sequence.range 1_000 10_000 ~stop:`exclusive)
       (Sequence.range 2 10 ~stop:`exclusive))
    (Sequence.cartesian_product
       (Sequence.range 100 1_000 ~stop:`exclusive)
       (Sequence.range 10 100 ~stop:`exclusive))
  |> Sequence.filter_map ~f:(fun (a, b) ->
    if a * b < 10_000
    then Some (a * b, U.undigits (U.digits (a * b) @ U.digits b @ U.digits a))
    else None)
  |> Sequence.filter_map ~f:(fun (prod, n) ->
    if Euler.Math.is_pandigital_nz n then Some prod else None)
  |> Sequence.to_list
  |> List.dedup_and_sort ~compare:Int.compare
  |> List.sum (module Int) ~f:Fn.id
;;

let solve () = compute () |> Int.to_string

(* Test *)

let%test_unit "pandigital" = [%test_eq: int] (compute ()) 45228
