(* Project Euler: Problem 82 *)

(*
   We will need the following files to run this program.
   - https://projecteuler.net/project/resources/p082_matrix.txt
*)

open Core

let parse_data data =
  List.map ~f:(fun l -> Str.split (Str.regexp ",") l) data
  |> List.map ~f:(List.map ~f:Int.of_string)
  |> List.transpose_exn
;;

let compute data =
  let aux_rightward prev crnt =
    List.folding_map (List.zip_exn prev crnt) ~init:Int.max_value ~f:(fun acc (p, c) ->
      let x = c + Int.min acc p in
      (x, x))
  in
  let aux_leftward crnt work =
    List.folding_map
      (List.zip_exn crnt work |> List.rev)
      ~init:(List.last_exn work)
      ~f:(fun acc (c, w) ->
        let x = Int.min (c + acc) w in
        (x, x))
    |> List.rev
  in
  let matrix = parse_data data in
  List.fold (List.tl_exn matrix) ~init:(List.hd_exn matrix) ~f:(fun prev crnt ->
    aux_rightward prev crnt |> aux_leftward crnt)
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn
;;

let solve fname = compute (Euler.Task.read_file fname) |> Int.to_string

(* Test *)

let%test_unit "p082_matrix.txt" =
  [%test_eq: int] (compute (Euler.Task.read_file "./assets/p082_matrix.txt")) 260324
;;
