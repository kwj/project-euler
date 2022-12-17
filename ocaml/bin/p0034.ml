(* Project Euler: Problem 34 *)

(*
  the maximum n-digits number is n * 9!

     10 ** (n-1) <= n * 9!
  -> n - 1 <= log10(n * 9!)
  -> n - 1 <= log10(n) + log10(9!)
  -> n - 1 <= log10(n) + 5.559
  -> n <= log10(n) + 6.559

    >>> math.log10(5)
    0.6989700043360189
    >>> math.log10(6)
    0.7781512503836436
    >>> math.log10(7)
    0.8450980400142568
    >>> math.log10(8)
    0.9030899869919435

  so, 'n' is 7 or less.

  9! * 7 = 362880 * 7 = 2540160
  if the first digit is '2' on 7-digits number, the maximum number of the remaing 6-digits is 999999 (6 * 9! = 2177280).
  so, 2nd-digit is 0 or 1. if 2nd-digit is 1, the maximum number 2! + 1! + 5*9! = 2 + 1 + 1814400 = 1814403.
  This is a contradiction, so the answer I look for is 1999999 or less.
 *)

open Core

(*
  let make_fact_table () =
    let rec fact n = if n = 0 then 1 else n * fact (n - 1) in
    let table = Array.create ~len:10 0 in
    let rec aux n =
      if n < 0 then
        table
      else (
        table.(n) <- fact n;
        aux (n - 1)
      )
    in
    aux 9
 *)

let solve () =
  let sum_of_fact num =
    let fact_table = [|1; 1; 2; 6; 24; 120; 720; 5040; 40320; 362880|] in    (* make_fact_table() *)
    let rec aux n sum =
      if n = 0 then sum else aux (n / 10) (sum + fact_table.(n mod 10))
    in
    aux num 0
  in
  let rec solve' n sum =
    if n < 10 then
      sum
    else
      if n = sum_of_fact n then
        solve' (pred n) (sum + n)
      else
        solve' (pred n) sum
  in
  solve' 1_999_999 0

let exec () =
  Int.to_string (solve ())

let () = Euler.Task.run exec
