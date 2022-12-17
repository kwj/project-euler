(* Project Euler: Problem 63 *)

(*
  n - 1 <= log10(m^n) < n    [m>0, n>0]
    --> n - 1 <= n * log10(m) < n
    --> m < 10
   and
    --> (n - 1)/n <= log10(m)
    --> 10 ^ (n - 1)/n <= 10 ^ log10(m)
    --> log10(10 ^ (n - 1)/n) <= log10(m)
    --> 10 ^ (n - 1)/n <= m
 *)

open Core

let solve () =
  let rec loop m result =
    if m < 1 then
      result
    else
      let rec aux n acc =
        if Float.((of_int 10) ** ((of_int n - 1.) / (of_int n)) <= of_int m) then
          aux (succ n) (succ acc)
        else
          acc
      in
      loop (pred m) (result + (aux 1 0))
  in
  loop 9 0

let exec () =
  Int.to_string (solve ())

let () = Euler.Task.run exec
