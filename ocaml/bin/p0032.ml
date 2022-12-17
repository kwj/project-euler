(* Project Euler: Problem 32 *)

(*
  m * n = mn (multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital)

  - numbers of digits of multiplicand/multiplier must be 4 or less.
  - if number of digits of multiplicand is 4, number of digits of multiplier is 1.
  - if number of digits of multiplicand is 3, number of digits of multiplier is 2.

  multiplicand/multiplier/product : 4-digits/1-digit/4-digits or 3-digits/2-digits/4-digits
 *)

open Core

let solve () =
  let module U = Euler.Util in
  (* 9-digits -> [(4-digits, 1-digit), 4-digits; (3-digits, 2-digits), 4-digits] *)
  let make_tpls lst =
    let lhs, rhs = List.split_n lst 5 in
    [List.split_n lhs 4, rhs; List.split_n lhs 3, rhs]
  in
  let confirm_requirement ((m1, m2), prod) =
    U.list_to_num m1 * U.list_to_num m2 = U.list_to_num prod
  in

  Euler.Util.permutation 9 [1; 2; 3; 4; 5; 6; 7; 8; 9]
  |> List.map ~f:make_tpls
  |> List.concat
  |> List.filter ~f:confirm_requirement
  |> List.map ~f:(fun (_, p) -> U.list_to_num p)
  |> List.dedup_and_sort ~compare
  |> List.fold ~init:0 ~f:(+)

let exec () =
  Int.to_string (solve ())

let () = Euler.Task.run exec
