(* Project Euler: Problem 42 *)

open Core

let parse_data data =
  let line = List.hd_exn data in
  String.sub line ~pos:1 ~len:(String.length line - 2)
  |> Str.split (Str.regexp {|","|})
  |> List.map ~f:(fun s ->
    List.init (String.length s) ~f:(String.get s)
    |> List.map ~f:(fun ch -> Char.to_int ch - 0x40) (* 0x41 = 'A', 0x42 = 'B', ... *)
    |> List.sum (module Int) ~f:Fn.id)
;;

let compute str_lst =
  parse_data str_lst |> List.count ~f:(fun n -> Euler.Math.is_triangular n)
;;

let solve fname =
  compute (Euler.Task.read_file fname) |> Int.to_string
;;

(* Test *)

let%test_unit "p042_words.txt" =
  [%test_eq: int] (compute (Euler.Task.read_file "./assets/p042_words.txt")) 162
;;
