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
  |> List.map ~f:(Array.of_list)

let solve arr_lst =
  let rec loop work_arr = function
    | [] -> Array.stable_sort work_arr ~compare;
            work_arr.(0)
    | arr :: xs ->
            let len = Array.length work_arr in
            work_arr.(0) <- work_arr.(0) + arr.(0);
            for i = 1 to len - 1 do
              work_arr.(i) <- min (work_arr.(i) + arr.(i)) (work_arr.(i - 1) + arr.(i))
            done;
            for i = len - 2 downto 0 do
              work_arr.(i) <- min work_arr.(i) (work_arr.(i + 1) + arr.(i))
            done;
            loop work_arr xs
  in
  loop (List.hd_exn arr_lst) (List.tl_exn arr_lst)


let exec data =
  Int.to_string (solve (parse_data data))

let () = Euler.Task.run_with_data exec (Euler.Task.read_data ())
