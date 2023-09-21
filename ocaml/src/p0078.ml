(* Project Euler: Problem 78 *)

(*
  p(5) = 7
  p(10) = 42
  p(50) = 204226
  p(100) = 190569292
  p(200) = 3972999029388
  p(500) = 2300165032574323995027
  p(1000) = 24061467864032622473692149727991
    ...

  I needed to find another way instead of dynamic programming.
  However, I gave up trying to solve it on my own.

  I saw following pages.

    https://en.wikipedia.org/wiki/Partition_(number_theory)
    https://en.wikipedia.org/wiki/Partition_function_(number_theory)
    https://en.wikipedia.org/wiki/Pentagonal_number_theorem

    p(n) = Sigma{k ∈ Z/{0}} (-1)^(k+1) * p(n - k(3k-1)/2)
         = p(n-1) + p(n-2) - p(n-5) - p(n-7) + p(n-12) + p(n-15) - p(n-22) - ...

      [p(0) = 1, p(k) = 0 when k < 0]

  I consider only value of 'mod 1_000_000' because the problem is divisible by one million or not.
*)

open Core

let penta i =
  let sign = [| -1; 1 |] in
  let k = sign.(i mod 2) * ((i + 1) / 2) in
  k * ((3 * k) - 1) / 2
;;

let right_side n buff =
  let sign = [| 1; 1; -1; -1 |] in
  let rec aux i result =
    let j = penta i in
    if j > n
    then result
    else aux (succ i) (result + (sign.((i - 1) mod 4) * Queue.get buff (n - j)))
  in
  aux 1 0
;;

let compute modulus =
  let buff = Queue.create () in
  let rec aux n =
    match (right_side n buff) mod modulus with
    | 0 -> n
    | x ->
      Queue.enqueue buff x;
      aux (succ n)
  in
  Queue.enqueue buff 1;
  aux 1
;;

let solve () = compute 1_000_000 |> Int.to_string

(* Test *)

let%test_unit "1_000_000" = [%test_eq: int] (compute 1_000_000) 55374

