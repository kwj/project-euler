(* Project Euler: Problem 82 *)

(*
   We will need the following files to run this program.
   - https://projecteuler.net/project/resources/p082_matrix.txt
*)

open Core

let parse_data data =
  data
  |> List.(map ~f:(Fun.compose (map ~f:Int.of_string) (String.split ~on:',')))
  |> List.transpose_exn
;;

let compute data =
  let aux_rightward prev crnt =
    List.(
      folding_map (zip_exn prev crnt) ~init:Int.max_value ~f:(fun acc (p, c) ->
        let x = c + Int.min acc p in
        (x, x)))
  in
  let aux_leftward crnt work =
    List.(
      folding_map
        (zip_exn crnt work |> rev)
        ~init:(last_exn work)
        ~f:(fun acc (c, w) ->
          let x = Int.min (c + acc) w in
          (x, x))
      |> rev)
  in
  let matrix = parse_data data in
  List.(
    fold (tl_exn matrix) ~init:(hd_exn matrix) ~f:(fun prev crnt ->
      aux_rightward prev crnt |> aux_leftward crnt)
    |> min_elt ~compare:Int.compare
    |> Option.value_exn)
;;

let solve fname = compute (Euler.Task.read_file fname) |> Int.to_string

(* Test *)

let%test_unit "p082_matrix.txt" =
  [%test_eq: int] (compute (Euler.Task.read_file "./assets/p082_matrix.txt")) 260324
;;
