(* Project Euler: Problem 73 *)

(*
  stupid algorithm

  the following looks interesting and I'll read and think about it later
    https://en.wikipedia.org/wiki/Farey_sequence#Next_term
 *)

(* ---------------------------------------------------------------- *)

let rec gcd m n =
  if n = 0 then m else gcd n (m mod n)

let solve limit =
  let rec loop' num den low result =
    if num <= low then
      result
    else
      match gcd den num with
      | 1 -> loop' (pred num) den low (succ result)
      | _ -> loop' (pred num) den low result
  in
  let rec loop n result =
    if n < 4 then
      result
    else
      loop (pred n) (result + (loop' (n / 2) n (n / 3) 0))
  in
  loop limit 0

let () =
  Printf.printf "Answer: %d\n" (solve 12_000)
