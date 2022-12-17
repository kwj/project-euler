(* Project Euler: Problem 3 *)

let prime_factors num =
  let rec aux i n =
    if n = 1 then
      []
    else
      if n mod i <> 0 then
        aux (succ i) n
      else
        match aux i (n / i) with
        | (base, exp) :: lst when base = i -> (base, (succ exp)) :: lst
        | lst -> (i, 1) :: lst
  in
  if num = 1 then [(1, 1)] else aux 2 num
  
let () =
  let a, _ = List.hd (List.rev (prime_factors 600851475143)) in
  Printf.printf "largest prime factor of the number 600851475143 is %d\n" a
