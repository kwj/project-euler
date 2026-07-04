(* Project Euler: Problem 42 *)

open Core

let parse_data data =
  let line = List.hd_exn data in
  List.(
    String.sub line ~pos:1 ~len:(String.length line - 2)
    (* If the Core library supports `split_all` or an equivalent feature, I'll use it. *)
    |> Stdlib.String.split_all ~sep:{|","|}
    |> map ~f:String.to_list
    |> map ~f:(sum (module Int) ~f:(fun ch -> Char.to_int ch - 0x40)))
;;

let compute str_lst = parse_data str_lst |> List.count ~f:Euler.Math.is_triangular
let solve fname = compute (Euler.Task.read_file fname) |> Int.to_string

(* Test *)

let%test_unit "p042_words.txt" =
  [%test_eq: int] (compute (Euler.Task.read_file "./assets/p042_words.txt")) 162
;;
