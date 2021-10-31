(* Project Euler: Problem 10 *)

(* This program is very SLOW! *)
(* Using *Set*, for fun *)

module IntSet = Set.Make(Int)

let sieve_of_eratosthenes num =
  let u_limit = truncate(sqrt (float num)) in
  let mult n = IntSet.of_list @@ List.tl @@ List.init (num / n) (fun x -> (x + 1) * n) in
  let next_num st n = IntSet.find_first_opt (fun e -> e > n) st in
  let rec aux st n =
    if n > u_limit then st else
      match next_num st n with
      | None -> st
      | Some v -> aux (IntSet.diff st (mult v)) v
  in
  IntSet.elements (aux (IntSet.of_list @@ List.init (pred num) (fun x -> x + 2)) 1)
  
let () =
  let primes = sieve_of_eratosthenes 2_000_000 in
  Printf.printf "the sum of all the primes below two million is %d\n" (List.fold_left (+) 0 primes);;
