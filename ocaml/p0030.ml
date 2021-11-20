(* Project Euler: Problem 30 *)

(*
  (1) 10 ** (n-1) <= n-digits number < 10 ** n
  (2) assume that x is sum of the fifth power of each digit on n-digits number
        n * (1**5) <= x <= n * (9**5) = n * 54049

     when n=6:
       6 * 54049 = 324294
     when n=7
       7 * 54049 = 378343 < 10 ** (7-1) = 1000000 (minimum 7-digits number)
       --> contradiction

   It's clear that 'x' is not a single digit number.
   so, we need to search 'x' in the follwing range:
     10 <= 'x' <= 354294 = 6 * (9 ** 5)
 *)

(*
  let make_nth_powers exp =
    let rec aux n lst =
      if n < 0 then
        lst
    else
      aux (pred n) ((n, (int_of_float (Float.pow (float_of_int n) (float_of_int exp)))) :: lst)
  in
  aux 9 []
 *)

let brute_force () =
  let sum_of_digits num =
    (* see above make_nth_powers() *)
    let a_pows = [(0, 0); (1, 1); (2, 32); (3, 243); (4, 1024); (5, 3125); (6, 7776); (7, 16807); (8, 32768); (9, 59049)] in
    let rec aux n result =
      if n = 0 then
        result
      else
        aux (n / 10) (result + (List.assoc (n mod 10) a_pows))
    in
    aux num 0
  in
  let rec aux n result =
    if n < 10 then
      result
    else
      if n = sum_of_digits n then
        aux (n - 1) (result + n)
      else
        aux (n - 1) result
  in
  aux 354294 0

let () =
  Printf.printf "the sum of all the numbers that can be written as \
                 the sum of fifth powers of their digits is %d\n" (brute_force())
