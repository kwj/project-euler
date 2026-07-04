(* Project Euler: Problem 99 *)

open Core

let parse_data data =
  List.(
    data
    |> map ~f:(Fun.compose (map ~f:Float.of_string) (String.split ~on:','))
    |> map ~f:(fun lst -> (nth_exn lst 0, nth_exn lst 1)))
;;

let compute str_lst =
  parse_data str_lst
  |> List.mapi ~f:(fun idx (b, e) -> (Float.(e *. log10 b), idx + 1))
  |> List.max_elt ~compare:(fun (f1, _) (f2, _) -> Float.compare f1 f2)
  |> Option.value_exn
  |> snd
;;

let solve fname = compute (Euler.Task.read_file fname) |> Int.to_string

(* Test *)

let%test_unit "p099_base_exp.txt" =
  [%test_eq: int] (compute (Euler.Task.read_file "./assets/p099_base_exp.txt")) 709
;;
