(* Project Euler: Problem 45 *)

(*
  T(n) = n(n+1) / 2    : 1 3 6 10 15 21 28 36 45 ...
  P(n) = n(3n-1) / 2   : 1 5 12 22 35 51 ...
  H(n) = n(2n-1)       : 1 6 16 28 45 ...

    T(1) = H(1), T(3) = H(2), T(5) = H(3), ...
      --> T(2k-1) = (2k-1)2k / 2
                  = 4k^2 - 2k / 2
                  = 2k^2 - k
                  = k(2k - 1)
                  = H(k)

    Hexagonal numbers are also triangle numbers.
    So I search for a hexagonal number which is also pentagonal number from n=144.
 *)

open Core

let rec find_hexnum n =
  let hex_num = n * (2 * n - 1) in
  if Euler.Math.is_pentagonal hex_num then hex_num else find_hexnum (succ n)

let exec () =
  Int.to_string (find_hexnum 144)

let () = Euler.Task.run exec
