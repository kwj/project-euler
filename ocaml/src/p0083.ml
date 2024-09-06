(* Project Euler: Problem 83 *)

(*
   I used Dijkstra's algorithm to solve.
   https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm

   We will need the following files to run this program.
   - https://projecteuler.net/project/resources/p083_matrix.txt
*)

open Core

let parse_data data =
  List.map ~f:(fun l -> Str.split (Str.regexp ",") l) data
  |> List.map ~f:(List.map ~f:Int.of_string)
  |> Array.of_list_map ~f:Array.of_list
;;

let make_neighbor_tbl x_size y_size =
  let tbl = Array.make_matrix ~dimx:x_size ~dimy:y_size [ (0, 0) ] in
  for x = 0 to x_size - 1 do
    for y = 0 to y_size - 1 do
      tbl.(x).(y)
      <- [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]
         |> List.filter ~f:(fun (x, _) -> x >= 0 && x < x_size)
         |> List.filter ~f:(fun (_, y) -> y >= 0 && y < y_size)
    done
  done;
  tbl
;;

let make_distance_tbl x_size y_size =
  Array.make_matrix ~dimx:x_size ~dimy:y_size Int.max_value
;;

let compute data =
  let arr_lst = parse_data data in
  let x_size = Array.length arr_lst
  and y_size = Array.length arr_lst.(0) in
  let nbr_tbl = make_neighbor_tbl x_size y_size
  and dist_tbl = make_distance_tbl x_size y_size in
  let module PQ =
    Euler.PrioQueue.Make (struct
      type t = int * (int * int)

      let compare x y = Int.compare (fst y) (fst x)
    end)
  in
  let pq = PQ.init () in

  PQ.insert pq (arr_lst.(0).(0), (0, 0));
  dist_tbl.(0).(0) <- arr_lst.(0).(0);
  let rec loop () =
    if PQ.is_empty pq
    then dist_tbl.(x_size - 1).(y_size - 1)
    else (
      let d, (i, j) = PQ.extract pq in
      let rec aux = function
        | [] -> ()
        | (x, y) :: xs ->
          let new_d = d + arr_lst.(x).(y) in
          (match new_d < dist_tbl.(x).(y) with
           | true ->
             dist_tbl.(x).(y) <- new_d;
             PQ.insert pq (new_d, (x, y));
             aux xs
           | false -> aux xs)
      in
      aux nbr_tbl.(i).(j);
      loop ())
  in
  loop ()
;;

let solve fname = compute (Euler.Task.read_file fname) |> Int.to_string

(* Test *)

let%test_unit "p083_matrix.txt" =
  [%test_eq: int] (compute (Euler.Task.read_file "./assets/p083_matrix.txt")) 425185
;;
