(* Project Euler: Problem 47 *)

(*
  I think it would be faster to use Sieve of Eratosthenes.
  However I didn't know the upper limit, I didn't use it.
 *)

(* ---------------------------------------------------------------- *)

let count_pf num =
  let ulimit = truncate @@ sqrt @@ float num in
  let rec div_all i n =
    if n mod i <> 0 || n = 1 then n else div_all i (n / i)
  in
  let rec aux i n result =
    if n <= 1 then
      result
    else
      if i > ulimit then
        succ result
      else
        if n mod i <> 0 then
          aux (succ i) n result
        else
          aux (succ i) (div_all i n) (succ result)
  in
  aux 2 num 0

let rec find_number n =
  if count_pf n <> 4 then
    find_number (n + 1)
  else
    if count_pf (n + 1) <> 4 then
      find_number (n + 2)
    else
      if count_pf (n + 2) <> 4 then
        find_number (n + 3)
      else
        if count_pf (n + 3) <> 4 then
          find_number (n + 4)
        else
          n

let () =
  Printf.printf "Answer: %d\n" (find_number 1)
