(* Project Euler: Problem 67 *)

open Core

let rec select_leaf lst =
  match lst with
  | [ x; y ] -> [ Int.max x y ]
  | x :: (y :: _ as tl) -> Int.max x y :: select_leaf tl
  | _ -> failwith "the length of list is lower than 2"
;;

let calc_from_bottom l_lst =
  List.fold_right
    (List.drop_last_exn l_lst)
    ~f:(fun a b -> List.map2_exn a (select_leaf b) ~f:( + ))
    ~init:(List.last_exn l_lst)
  |> List.hd_exn
;;

let parse_data data =
  List.map
    ~f:(fun s -> Str.split (Str.regexp " ") s |> List.map ~f:(fun s -> Int.of_string s))
    data
;;

let compute str_lst = parse_data str_lst |> calc_from_bottom
let solve fname = compute (Euler.Task.read_file fname) |> Int.to_string

(* Test *)

let%test_unit "p067_triangle.txt" =
  [%test_eq: int] (compute (Euler.Task.read_file "./assets/p067_triangle.txt")) 7273
;;
