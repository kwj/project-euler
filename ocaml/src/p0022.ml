(* Project Euler: Problem 22 *)

open Core

let parse_data data =
  let line = List.hd_exn data in
  List.(
    String.(sub line ~pos:1 ~len:(length line - 2))
    (* If the Core library supports `split_all` or an equivalent feature, I'll use it. *)
    |> Stdlib.String.split_all ~sep:{|","|}
    |> sort ~compare:String.ascending
    |> map ~f:String.to_list
    |> map ~f:(List.sum (module Int) ~f:(fun ch -> Char.to_int ch - 0x40)))
;;

let compute str_lst =
  parse_data str_lst |> List.foldi ~init:0 ~f:(fun idx acc n -> acc + ((idx + 1) * n))
;;

let solve fname = compute (Euler.Task.read_file fname) |> Int.to_string

(* Test *)

let%test_unit "p022_names.txt" =
  [%test_eq: int] (compute (Euler.Task.read_file "./assets/p022_names.txt")) 871198282
;;
