(* Project Euler: Problem 17 *)

open Core

let words =
  [ (1, String.length "one")
  ; (2, String.length "two")
  ; (3, String.length "three")
  ; (4, String.length "four")
  ; (5, String.length "five")
  ; (6, String.length "six")
  ; (7, String.length "seven")
  ; (8, String.length "eight")
  ; (9, String.length "nine")
  ; (10, String.length "ten")
  ; (11, String.length "eleven")
  ; (12, String.length "twelve")
  ; (13, String.length "thirteen")
  ; (14, String.length "fourteen")
  ; (15, String.length "fifteen")
  ; (16, String.length "sixteen")
  ; (17, String.length "seventeen")
  ; (18, String.length "eighteen")
  ; (19, String.length "nineteen")
  ; (20, String.length "twenty")
  ; (30, String.length "thirty")
  ; (40, String.length "forty")
  ; (50, String.length "fifty")
  ; (60, String.length "sixty")
  ; (70, String.length "seventy")
  ; (80, String.length "eighty")
  ; (90, String.length "ninety")
  ; (0, 0) (* special data *)
  ]
;;

let count_letters num =
  match num with
  | 1000 -> 11 (* one thousand *)
  | n when n < 20 -> List.Assoc.find_exn words n ~equal
  | n when n < 100 ->
    List.Assoc.find_exn words (n - (n mod 10)) ~equal
    + List.Assoc.find_exn words (n mod 10) ~equal
  | n when n mod 100 = 0 ->
    (* X00 hundred *)
    List.Assoc.find_exn words (n / 100) ~equal + 7
  | n when n mod 100 < 20 ->
    (* X hundred and YZ (YZ < 20) *)
    List.Assoc.find_exn words (n / 100) ~equal
    + 7
    + 3
    + List.Assoc.find_exn words (n mod 100) ~equal
  | n ->
    (* X hundred and YZ (20 <= YZ <= 99) *)
    List.Assoc.find_exn words (n / 100) ~equal
    + 7
    + 3
    + List.Assoc.find_exn words ((n mod 100) - (n mod 10)) ~equal
    + List.Assoc.find_exn words (n mod 10) ~equal
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
