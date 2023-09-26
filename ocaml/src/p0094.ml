(* Project Euler: Problem 94 *)

(*
 * a,a,b
 * --> 'b' must be even since area of triangle is integer.
 *       Heron's formula: triangle area S =sqrt(s*(s-a)*(s-b)*(s-c)) [s=(a+b+c)/2, a/b/c are length of sides]
 *       s = (a + a + b)/2 = (2a + b)/2
 *       S = sqrt( (2a+b)/2 * b/2 * b/2 * (2a-b)/2 )
 *         = sqrt( (b^2 * (4a^2 - b^2)) / 16 )
 *         = b/4 * sqrt(4a^2-b^2)
 *
 * m,m,2n  [m=2n+1 or 2n-1]
 * -->
 *   [when m=2n+1]
 *   s = (2(2n+1) + 2n)/2 = 3n + 1
 *   S = sqrt( (3n+1)*(3n+1 - (2n+1))*(3n+1 - (2n+1))*(3n+1 - 2n) )
 *     = sqrt( (3n+1)*n*n*(n+1) )
 *     = n * sqrt( (3n+1)(n+1) )
 *
 *     sqrt(...) must be an integer, so k^2 = (3n+1)(n+1)
 *       k^2 = 3n^2 + 4n + 1
 *       3k^2 = 9n^2 + 12n + 3
 *            = (3n + 2)^2 - 1
 *       (3n + 2)^2 -3k^2 = 1
 *       x^2 - 3y^2 = 1  [x=3n+2,y=k]
 *
 *   perimeter = m + m + 2n = 6n + 2 = 2*(3n + 2) - 2 = 2x - 2
 *
 *   [when m=2n-1]
 *   s = (2(2n-1) + 2n)/2 = 3n - 1
 *   S = sqrt( (3n-1)*(3n-1 - (2n-1))*(3n-1 - (2n-1))*(3n-1 - 2n) )
 *     = sqrt( (3n-1)*n*n*(n-1) )
 *     = n * sqrt( (3n-1)(n-1) )
 *
 *     sqrt(...) must be an integer, so k^2 = (3n-1)(n-1)
 *       k^2 = 3n^2 - 4n + 1
 *       3k^2 = 9n^2 - 12n + 3
 *            = (3n - 2)^2 - 1
 *       (3n - 2)^2 -3k^2 = 1
 *       x^2 - 3y^2 = 1  [x=3n-2,y=k]
 *
 *   perimeter = m + m + 2n = 6n - 2 = 2*(3n - 2) + 2 = 2x + 2
 *
 * Pell's equation: x^2 -3y^2 = 1
 *   sqrt(3) = [1;(1,2)]  (from problem 64)
 *     a{i} + b{i}*sqrt(3) = (2 + 1*sqrt(3))^i  (from problem 66)
 *     --> a{1} = 2, b{1} = 1  ==> a mod 3 = 2, 3n+2 = 2 -> n=0  (ignore)
 *         a{2} = 7, b{2} = 3  ==> a mod 3 = 1, 3n-2 = 7 -> n=3,m=5, p = 2 * a{2} + 2 = 16
 *           ...
 *)

open Core

let compute limit =
  let rec loop a b result =
    let next_a, next_b = ((2 * a) + (3 * b), a + (2 * b)) in
    match next_a mod 3 with
    | 2 ->
      let p = (2 * next_a) - 2 in
      if p < limit then loop next_a next_b (result + p) else result
    | 1 ->
      let p = (2 * next_a) + 2 in
      if p < limit then loop next_a next_b (result + p) else result
    | _ -> failwith "must not be reachable! (it must be logic error)"
  in
  loop 2 1 0 (* a{1}=2, b{1}=1 *)
;;

let solve () = compute 1_000_000_000 |> Int.to_string

(* Test *)

let%test_unit "1_000_000_000" = [%test_eq: int] (compute 1_000_000_000) 518408346
