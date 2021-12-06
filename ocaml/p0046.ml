(* Project Euler: Problem 46 *)

(*
  odd composite = prime? + 2 * n^2
 *)

(* ---------------------------------------------------------------- *)

let is_prime num =
  let upper = truncate @@ sqrt @@ float_of_int num in
  let rec aux n k =
    if k < 2 then
      true
    else
      (n mod k <> 0) && (aux n (k - 1))
  in
  if num <= 1 then false else aux num upper

let is_twice_square num =
  let tmp = sqrt @@ float_of_int (num / 2) in
  if tmp = floor tmp then true else false

let rec find_counter_example n primes =
  if is_prime n then
    find_counter_example (n + 2) (n :: primes)
  else
    let rec aux lst =
      match lst with
      | hd :: tl when is_twice_square (n - hd) = true -> find_counter_example (n + 2) primes
      | hd :: tl -> aux tl
      | [] -> n
    in
    aux primes

let () =
  (* '9' is the smallest odd composite number. '[7;5;3]' are odd prime numbers less than '9'. *)
  Printf.printf "Answer: %d\n" (find_counter_example 9 [7; 5; 3])
