(* Project Euler: Problem 51 *)

(*
  the smallest prime which, by replacing part of the number with same digit,
  is part of an eight prime value family

  -> eight numbers out of '0' to '9' are used for replacement

  1) the last digit is not eligible for replacement
    It make some even numbers after replacement.

  2) the number of digits of the prime numbers is greater than number of digits to be replaced
    the reason is since 1).

  3) the number of digits that can be replaced is only a multiples of 3
    if 'n' is not multiples of 3, some replaced numbers will contain multiples of 3.

      number  digits  sum  'mod 3'    [n>0]
      ---------------------------------------------
      0       n       0    0
      1       n       n    n mod 3
      2       n       2n   2n mod 3
      3       n       3n   3n mod 3 = 0
      4       n       4n   4n mod 3 = n mod 3
      5       n       5n   5n mod 3 = 2n mod 3
      6       n       6n   6n mod 3 = 0
      7       n       7n   7n mod 3 = n mod 3
      8       n       8n   8n mod 3 = 2n mod 3
      9       n       9n   9n mod 3 = 0

  I want to use prime number tables to speed up and simplify. For the
  moment, I assume that prime numbers less than one million. The reason
  is that I want to only consider the case of replacing three digits.

  4) There are at least same three numbers other than last digit.

 *)

open Core

let is_prime = Euler.Math.mr_isprime

(*
  # get_patterns 6;;
  - : int list list =
  [[0; 1; 1; 1; 0; 0]; [0; 1; 1; 0; 1; 0]; [0; 1; 1; 0; 0; 1];
   [0; 1; 0; 1; 1; 0]; [0; 1; 0; 1; 0; 1]; [0; 1; 0; 0; 1; 1];
   [0; 0; 1; 1; 1; 0]; [0; 0; 1; 1; 0; 1]; [0; 0; 1; 0; 1; 1];
   [0; 0; 0; 1; 1; 1]]

  [LSD; ...; MSD] : LSD - Least Significant Digit, MSD - Most Significant Digit
 *)
let get_patterns n_digits =
  let select_bits () =
    let rec aux acc = function
      | x :: xs -> aux ((Euler.Util.combination x (List.range 1 n_digits)) :: acc) xs
      | [] -> List.concat acc
    in
    aux [] (List.range ~stride:3 3 n_digits)
  in
  let flip_bits bit_lst =
    let arr = Array.create ~len:n_digits 0 in
    let rec aux = function
      | x :: xs -> arr.(x) <- 1; aux xs
      | [] -> Array.to_list arr
    in
    aux bit_lst
  in
  List.map ~f:flip_bits (select_bits ())

(*
  # is_match 121213 [0; 1; 0; 1; 0; 1];;
  - : int list option = Some [0; 1; 0; 1; 0; 1]
  # is_match 121213 [0; 1; 0; 1; 1; 0];;
  - : int list option = None
 *)
let is_match prime pat =
  let msd = List.last_exn pat in  (* msd: most significant digit *)
  let rec aux acc n = function
    | x :: xs when x = 1 -> aux ((n mod 10) :: acc) (n / 10) xs
    | _ :: xs -> aux acc (n / 10) xs
    | [] -> List.dedup_and_sort acc ~compare
  in
  let num_lst = aux [] prime pat in
  if List.length num_lst <> 1 then
    None
  else
    if List.hd_exn num_lst > (2 + msd) then None else Some pat

let check_prime_group prime pat =
  let module U = Euler.Util in
  let mask_lst = List.map2_exn (U.int_to_nlst prime) pat ~f:(fun a b -> if b = 0 then a else -1) in
  let prime_group = List.map (List.range (List.last_exn pat) 10)
                      ~f:(fun x -> List.map mask_lst ~f:(fun m -> if m = -1 then x else m))
                    |> List.map ~f:U.nlst_to_int
                    |> List.filter ~f:is_prime in
  if List.length prime_group = 8 then Some prime_group else None

let solve () =
  let rec loop n_digits =
    let primes = Euler.Eratosthenes.to_list (Euler.Eratosthenes.generate (Int.pow 10 n_digits))
                 |> List.filter ~f:(fun n -> n > Int.pow 10 (n_digits - 1)) in
    let master_pat_lst = get_patterns n_digits in
    let rec aux = function
      | [] -> loop (succ n_digits)
      | x :: xs ->
          match List.filter_map master_pat_lst ~f:(is_match x) with
          | [] -> aux xs
          | pat_lst ->
              match List.filter_map pat_lst ~f:(check_prime_group x) with
              | [] -> aux xs
              | l -> List.hd_exn l
    in
    aux primes
  in
  loop 4 |> List.hd_exn

let exec () =
  Int.to_string (solve ())

let () = Euler.Task.run exec
