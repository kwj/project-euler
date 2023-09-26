(* Project Euler: Problem 69 *)

(*
 * https://en.wikipedia.org/wiki/Euler%27s_totient_function
 *
 *   n/phi(n) = n / n(1-1/p{1})(1-1/p{2})...(1-1/p{r})
 *            = p{1}p{2}...p{r} / (p{1}-1)(p{2}-1)...(p{r}-1)
 *                   (p{i} is prime number)
 *
 * the above show that value of n/phi(n) depends on the prime factors of 'n'.
 *
 * generally, 1 < m/(m-1) and m/(m-1) > n/(n-1) [m<n].
 *
 * so I'll find the maximum 'k' which satisfies follwing condition.
 *
 *   p{1} * p{2} * ... * p{k-1} * p{k} <= 1_000_000
 *      [p{i} is prime number: 2, 3, 5, 7, ...]
 *
 * the answer 'n' is p{1} * p{2} * ... * p{k-1} * p{k}.
 *)

open Core

let compute limit =
  let rec loop p ans =
    let prime = Euler.Math.Prime.next_prime p in
    if prime * ans > limit then ans else loop prime (ans * prime)
  in
  loop 1 1
;;

let solve () = compute 1_000_000 |> Int.to_string

(* Test *)

let%test_unit "10" = [%test_eq: int] (compute 10) 6
let%test_unit "1_000_000" = [%test_eq: int] (compute 1_000_000) 510510
