(* Project Euler: Problem 41 *)

(*
  (sum_{1}_{9} n) mod 3 = 45 mod 3 = 0  --> 9 digits pandigital number is a multiple of 3, not a prime.
  (sum_{1}_{8} n) mod 3 = 36 mod 3 = 0  --> 8 digits pandigital number is a multiple of 3, not a prime.
  (sum_{1}_{7} n) mod 3 = 28 mod 3 = 1
  (sum_{1}_{6} n) mod 3 = 21 mod 3 = 0  --> 6 digits pandigital number is a multiple of 3, not a prime.
  (sum_{1}_{5} n) mod 3 = 15 mod 3 = 0  --> 5 digits pandigital number is a multiple of 3, not a prime.
  (sum_{1}_{4} n) mod 3 = 10 mod 3 = 1
 *)

(* ---------------------------------------------------------------- *)

let is_pandigital number =
  let mk_bits num =
    let rec aux n bits =
      if n = 0 then bits else aux (n / 10) (bits lor (1 lsl ((n mod 10) - 1)))
    in
    aux num 0
  in
  let ndigits = String.length (string_of_int number) in
  mk_bits number = ((1 lsl ndigits) - 1)

let is_prime num =
  let upper = truncate @@ sqrt @@ float num in
  let rec aux n k =
    if k < 2 then true else (n mod k <> 0) && (aux n (k - 1))
  in
  if num <= 1 then false else aux num upper

let solve n =
  let rec aux num =
    match num with
    | n when 999999 < n && n <= 9999999    (* 7 digits *)
      -> if n mod 3 = 0 || n mod 5 = 0 then aux (n - 2) else
           if is_pandigital n && is_prime n then n else aux (n - 2)
    | n when 999 < n && n <= 9999    (* 4 digits *)
      -> if n mod 3 = 0 || n mod 5 = 0 then aux (n - 2) else
           if is_pandigital n && is_prime n then n else aux (n - 2)
    | n when 9999 < n && n <= 999999    (* skip if the number is 5 or 6 digits *)
      -> aux 9999
    | _    (* not reached it on this problem *)
      -> 0
  in
  let start = if n mod 2 = 0 then n - 1 else n in
  aux start

let () =
  Printf.printf "Answer: %d\n" (solve 7654321)
