(* Project Euler: Problem 81 *)

(*
   We will need the following files to run this program.
   - https://projecteuler.net/project/resources/p081_matrix.txt
*)

open Core

let parse_data data =
  data |> List.(map ~f:(Fun.compose (map ~f:Int.of_string) (String.split ~on:',')))
;;

let compute data =
  let aux_rightward prev crnt =
    List.(
      folding_map (zip_exn prev crnt) ~init:Int.max_value ~f:(fun acc (p, c) ->
        let x = c + Int.min acc p in
        (x, x)))
  in
  let matrix = parse_data data in
  List.(
    fold
      (tl_exn matrix)
      ~init:(folding_map (hd_exn matrix) ~init:0 ~f:(fun acc x -> (acc + x, acc + x)))
      ~f:aux_rightward
    |> last_exn)
;;

let solve fname = compute (Euler.Task.read_file fname) |> Int.to_string

(* Test *)

let%test_unit "p081_matrix.txt" =
  [%test_eq: int] (compute (Euler.Task.read_file "./assets/p081_matrix.txt")) 427337
;;
