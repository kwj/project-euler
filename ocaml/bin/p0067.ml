(* Project Euler: Problem 67 *)

(*
  We'll need the following files to run this program.
    - https://projecteuler.net/project/resources/p067_triangle.txt
 *)

open Core

let parse_data data =
  List.map ~f:(fun l -> Str.split (Str.regexp " ") l |> List.map ~f:(fun s -> Int.of_string s)) data

let calc_from_bottom l_lst =
  let add_max_leaf a b =
    let rec select_leaf lst =
      match lst with
      | x :: y :: [] -> [max x y]
      | x :: (y :: _ as tl) -> (max x y) :: (select_leaf tl)
      | _ -> assert false
    in
    List.map2_exn a (select_leaf b) ~f:(+)
  in
  List.fold_right l_lst
    ~f:add_max_leaf
    ~init:(List.init ((List.length @@ List.hd_exn @@ List.rev l_lst) + 1) ~f:(fun _ -> 0))
  |> List.hd_exn

let exec data =
  Int.to_string (calc_from_bottom (parse_data data))

let () = Euler.Task.run_with_data exec (Euler.Task.read_data ())
