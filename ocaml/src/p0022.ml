(* Project Euler: Problem 22 *)

open Core

let parse_data data =
  let line = List.hd_exn data in
  String.sub line ~pos:1 ~len:(String.length line - 2)
  |> Str.split (Str.regexp {|","|})
  |> List.sort ~compare:String.compare
  |> List.map ~f:(fun s ->
    List.init (String.length s) ~f:(String.get s)
    |> List.map ~f:(fun ch -> Char.to_int ch - 0x40) (* 0x41 = 'A', 0x42 = 'B', ... *)
    |> List.sum (module Int) ~f:Fn.id)
;;

let compute str_lst =
  parse_data str_lst |> List.foldi ~init:0 ~f:(fun idx acc n -> acc + ((idx + 1) * n))
;;

let solve () =
  compute (Euler.Task.read_data "./src/assets/p022_names.txt") |> Int.to_string
;;

(* Test *)

let%test_unit "p022_names.txt" =
  [%test_eq: int] (compute (Euler.Task.read_file "./assets/p022_names.txt")) 871198282
;;
