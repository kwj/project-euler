(* Project Euler: Problem 17 *)

open Core

let under_20 =
  [ 0
  ; String.length "one"
  ; String.length "two"
  ; String.length "three"
  ; String.length "four"
  ; String.length "five"
  ; String.length "six"
  ; String.length "seven"
  ; String.length "eight"
  ; String.length "nine"
  ; String.length "ten"
  ; String.length "eleven"
  ; String.length "twelve"
  ; String.length "thirteen"
  ; String.length "fourteen"
  ; String.length "fifteen"
  ; String.length "sixteen"
  ; String.length "seventeen"
  ; String.length "eighteen"
  ; String.length "nineteen"
  ]
;;

let mults_10 =
  [ 0
  ; 0
  ; String.length "twenty"
  ; String.length "thirty"
  ; String.length "forty"
  ; String.length "fifty"
  ; String.length "sixty"
  ; String.length "seventy"
  ; String.length "eighty"
  ; String.length "ninety"
  ]
;;

let nchars_1000 = String.length "thousand"
let nchars_100 = String.length "hundred"
let nchars_and = String.length "and"

let rec count_letters = function
  | 1000 -> List.nth_exn under_20 1 + nchars_1000
  | n when n < 20 -> List.nth_exn under_20 n
  | n when n < 100 ->
    let q = n / 10
    and r = n mod 10 in
    List.nth_exn mults_10 q + List.nth_exn under_20 r
  | n ->
    let q = n / 100
    and r = n mod 100 in
    List.nth_exn under_20 q
    + nchars_100
    + if r = 0 then 0 else nchars_and + count_letters r
;;

let compute limit =
  List.range 1 limit ~stop:`inclusive
  |> List.map ~f:(fun n -> count_letters n)
  |> List.sum (module Int) ~f:Fn.id
;;

let solve () = compute 1_000 |> Int.to_string

(* Test *)

let%test_unit "5" = [%test_eq: int] (compute 5) 19
let%test_unit "1_000" = [%test_eq: int] (compute 1_000) 21124
