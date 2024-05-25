(* Project Euler: Problem 81 *)

(*
   We will need the following files to run this program.
   - https://projecteuler.net/project/resources/p081_matrix.txt
*)

open Core

let parse_data data =
  List.map ~f:(fun l -> Str.split (Str.regexp ",") l) data
  |> List.map ~f:(fun l -> Array.of_list (Int.max_value :: List.map ~f:Int.of_string l))
;;

let compute data =
  let rec loop work_arr = function
    | [] -> work_arr.(Array.length work_arr - 1)
    | arr :: xs ->
      for i = 1 to Array.length arr - 1 do
        if arr.(i - 1) < work_arr.(i)
        then arr.(i) <- arr.(i) + arr.(i - 1)
        else arr.(i) <- arr.(i) + work_arr.(i)
      done;
      loop arr xs
  in
  let init_arr arr =
    for i = 2 to Array.length arr - 1 do
      arr.(i) <- arr.(i) + arr.(i - 1)
    done;
    arr
  in
  let arr_lst = parse_data data in
  loop (init_arr (List.hd_exn arr_lst)) (List.tl_exn arr_lst)
;;

let solve fname =
  compute (Euler.Task.read_file fname) |> Int.to_string
;;

(* Test *)

let%test_unit "p081_matrix.txt" =
  [%test_eq: int] (compute (Euler.Task.read_file "./assets/p081_matrix.txt")) 427337
;;
