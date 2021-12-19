(* Project Euler: Problem 63 *)

(*
  n - 1 <= log10(m^n) < n    [m>0, n>0]
    --> n - 1 <= n * log10(m) < n
    --> m < 10
   and
    --> (n - 1)/n <= log10(m)
    --> log10(10 ^ ((n - 1)/n)) <= log10(m)m
    --> 10 ^ ((n - 1)/n) <= m
 *)

(* ---------------------------------------------------------------- *)

let solve () =
  let rec loop m result =
    if m < 1 then
      result
    else
      let rec aux n acc =
        if Float.(pow 10. (((of_int n) -. 1.) /. (of_int n)) <= of_int m) then
          aux (succ n) (succ acc)
        else
          acc
      in
      loop (pred m) (result + (aux 1 0))
  in
  loop 9 0

let () =
  Printf.printf "Answer: %d\n" (solve())
