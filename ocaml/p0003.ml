(* Project Euler: Problem 3 *)

let prime_factors num =
  let rec aux_fn1 i n =
    if n = 1 then [] else
      if n mod i = 0 then
        match aux_fn1 i (n / i) with
          (base, exp) :: lst when base = i -> (base, exp + 1) :: lst
        | lst -> (i, 1) :: lst
      else
        aux_fn1 (i + 1) n in
  let aux_fn2 n =
    match aux_fn1 2 n with
      [] -> [(1, 1)]
    | lst -> lst in
  aux_fn2 num;;
  
let () =
  let a, _ = List.hd (List.rev (prime_factors 600851475143)) in
  Printf.printf "largest prime factor of the number 600851475143 is %d\n" a;;
