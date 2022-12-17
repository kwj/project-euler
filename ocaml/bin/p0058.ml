(* Project Euler: Problem 58 *)

(*
  37 36 35 34 33 32 31
  38 17 16 15 14 13 30
  39 18  5  4  3 12 29
  40 19  6  1  2 11 28
  41 20  7  8  9 10 27
  42 21 22 23 24 25 26
  43 44 45 46 47 48 49
            |  |  |  |
         (n=0, 1, 2, 3, ...)

  edge length:
    2n+1

  sum of numbers lying along both diagonals:
    4n + 1

  each numbers in the four corners: (P:Prime, C:Composite)

  n=0  1,
  n=1        (1+2),     (1+2+2),     (1+2+2+2),     a1=(1+2+2+2+2),
  n=2        (a1+4),    (a1+4+4),    (a1+4+4+4),    a2=(a1+4+4+4+4),
  n=3        (a2+6),    (a2+6+6),    (a2+6+6+6),    a3=(a2+6+6+6+6),
             ...
  0-----------------------------------------------------------------
      NonP,  P or C     P or C       P or C         C (square number)
 *)

open Core

let solve () =
  let is_prime_wrapper num =
    if Euler.Math.mr_isprime num then 1 else 0
  in
  let rec aux n nprimes acc =
    let nprimes = nprimes
                  + is_prime_wrapper(acc + 2 * n)
                  + is_prime_wrapper(acc + 4 * n)
                  + is_prime_wrapper(acc + 6 * n) in
    if Float.(of_int nprimes / of_int Int.(4 * n + 1) <= 0.1) then
      2 * n + 1
    else
      aux (succ n) nprimes (acc + 8 * n)
  in
  aux 1 0 1

let exec () =
  Int.to_string (solve ())

let () = Euler.Task.run exec

