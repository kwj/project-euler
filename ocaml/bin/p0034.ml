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

  assume that a 7-digits number '1 d_{1} .. d{6}', any d_{i} >= 5. it becomes 1! + sum(d_{i}!) mod 10 = 1. it's a contradiction.
  so, at least one d_{i} is equal or less than 4. 1! + 4! + 5 * 9! = 1814425. 1! + 8! + 4! + 4 * 9! = 1491865.


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

  --> [|1; 1; 2; 6; 24; 120; 720; 5040; 40320; 362880|]
 *)

open Core

let solve limit =
  let memo = Array.create ~len:(limit + 1) 0 in
  let rec loop n sum =
    if n > limit then
      sum
    else (
      memo.(n) <- memo.(n / 10) + memo.(n mod 10);
      if n = memo.(n) then loop (succ n) (sum + n) else loop (succ n) sum
    )
  in
  memo.(0) <- 1; memo.(1) <- 1; memo.(2) <- 2; memo.(3) <- 6; memo.(4) <- 24;
  memo.(5) <- 120; memo.(6) <- 720; memo.(7) <- 5040; memo.(8) <- 40320; memo.(9) <- 362880;
  loop 10 0

let exec () =
  Int.to_string (solve 1_491_865)

let () = Euler.Task.run exec
