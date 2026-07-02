(* Project Euler: Problem 17 *)

open Core

let under_20 =
  List.map
    ~f:String.length
    [ ""
    ; "one"
    ; "two"
    ; "three"
    ; "four"
    ; "five"
    ; "six"
    ; "seven"
    ; "eight"
    ; "nine"
    ; "ten"
    ; "eleven"
    ; "twelve"
    ; "thirteen"
    ; "fourteen"
    ; "fifteen"
    ; "sixteen"
    ; "seventeen"
    ; "eighteen"
    ; "nineteen"
    ]
;;

let mults_10 =
  List.map
    ~f:String.length
    [ ""
    ; ""
    ; "twenty"
    ; "thirty"
    ; "forty"
    ; "fifty"
    ; "sixty"
    ; "seventy"
    ; "eighty"
    ; "ninety"
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
  List.(range 1 limit ~stop:`inclusive |> sum (module Int) ~f:count_letters)
;;

let solve () = compute 1_000 |> Int.to_string

(* Test *)

let%test_unit "5" = [%test_eq: int] (compute 5) 19
let%test_unit "1_000" = [%test_eq: int] (compute 1_000) 21124
