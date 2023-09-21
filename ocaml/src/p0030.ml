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
  We need to search 'x' in the follwing range:
    10 <= 'x' <= 354294 = 6 * (9 ** 5)

  We have two approaches to solve this problem. One is to search from left hand side,
  and the other is to search from right hand side.

  1) search from LHS

    # an example in Python
    def search_from_lhs():
        limit = 354_294
        memo_tbl = [0, 1, 32, 243, 1024, 3125, 7776, 16807, 32768, 59049] + [0] * (limit + 1 - 10)
        acc = 0
        for n in range(10, limit + 1):
            memo_tbl[n] = memo_tbl[n // 10] + memo_tbl[n % 10]
            if n == memo_tbl[n]:
                acc += n

        return acc

  2) search from RHS
    We search from combinations of numbers.
*)

open Core

let get_max_ndigits exp =
  let k = Int.pow 9 exp in
  let rec aux x =
    if x * k <= Int.pow 10 (x - 1) then (pred x) else aux (succ x)
  in
  aux 2
;;

let compute exp =
  let pow_tbl =
    List.range 0 9 ~stop:`inclusive
    |> List.map ~f:(fun n -> Int.pow n exp)
    |> List.to_array
  in
  List.range 2 (get_max_ndigits exp) ~stop:`inclusive
  |> List.map ~f:(fun n ->
    let rec aux ans = function
      | [] -> ans
      | xs :: xss ->
        let tmp =
          List.map ~f:(fun n -> pow_tbl.(n)) xs |> List.sum (module Int) ~f:Fn.id
        in
        if List.equal
             Int.equal
             (Euler.Util.digits tmp |> List.sort ~compare:Int.compare)
             xs
        then aux (ans + tmp) xss
        else aux ans xss
    in
    aux 0 (Euler.Util.combination_with_repetition n [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]))
  |> List.sum (module Int) ~f:Fn.id
;;

let solve () = compute 5 |> Int.to_string

(* Test *)

let%test_unit "4" = [%test_eq: int] (compute 4) 19316
let%test_unit "5" = [%test_eq: int] (compute 5) 443839
