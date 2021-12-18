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

  each numbers in the four corners:
  n=0  1,
  n=1        (1+2),     (1+2+2),     (1+2+2+2),     a1=(1+2+2+2+2),
  n=2        (a1+4),    (a1+4+4),    (a1+4+4+4),    a2=(a1+4+4+4+4),
  n=3        (a2+6),    (a2+6+6),    ...
             ...
  0-----------------------------------------------------------------
      NonP,  P or NonP, P or NonP,   P or NonP,     NonP (square number)
 *)

(* ---------------------------------------------------------------- *)

let solve () =
  let is_prime num =
    let rec aux n k =
      if k < 2 then
        true
      else
        (n mod k <> 0) && (aux n (k - 2))
    in
    if num mod 2 = 0 then
      false
    else
      if num <= 1 then
        false
      else
        let upper = truncate @@ sqrt @@ float num in
        if upper mod 2 = 0 then
          aux num (upper + 1)
        else
          aux num upper
  in
  let is_prime_wrapper num =
    if is_prime num = true then 1 else 0
  in
  let rec aux n nprimes acc =
    let nprimes = nprimes
                  + is_prime_wrapper(acc + 2 * n)
                  + is_prime_wrapper(acc + 4 * n)
                  + is_prime_wrapper(acc + 6 * n) in
    if (float nprimes) /. (float (4 * n + 1)) <= 0.1 then
      2 * n + 1
    else
      aux (succ n) nprimes (acc + 8 * n)
  in
  aux 1 0 1

let () =
  Printf.printf "Answer: %d\n" (solve ())
