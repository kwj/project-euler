(* Project Euler: Problem 32 *)

(*
  m * n = mn (multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital)

  - numbers of digits of multiplicand/multiplier must be 4 or less.
  - if number of digits of multiplicand is 4, number of digits of multiplier is 1.
  - if number of digits of multiplicand is 3, number of digits of multiplier is 2.

  multiplicand/multiplier/product : 4-digits/1-digit/4-digits or 3-digits/2-digits/4-digits
 *)

open Core

let make_case_1 () =
  let lst = ref [] in
  for m1 = 1_000 to 9_999 do
    for m2 = 2 to 9 do
      let prod = m1 * m2 in
      if prod < 10_000 then
        lst := (m1 * (Int.pow 10 5) + m2 * (Int.pow 10 4) + prod, prod) :: !lst
    done
  done;
  !lst

let make_case_2 () =
  let lst = ref [] in
  for m1 = 100 to 999 do
    for m2 = 10 to 99 do
      let prod = m1 * m2 in
      if prod < 10_000 then
        lst := (m1 * (Int.pow 10 6) + m2 * (Int.pow 10 4) + prod, prod) :: !lst
    done
  done;
  !lst

let solve () =
  let _, prod_lst = List.concat [make_case_1 (); make_case_2 ()]
                    |> List.filter ~f:(fun (n, _) -> Euler.Math.is_pandigital_nz n)
                    |> List.unzip
  in
  List.dedup_and_sort ~compare prod_lst |> List.fold ~init:0 ~f:(+)

let exec () =
  Int.to_string (solve ())

let () = Euler.Task.run exec
