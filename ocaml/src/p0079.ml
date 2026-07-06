(* Project Euler: Problem 79 *)

(*
   If you look at the log file, you can assume the answer with pen and paper :).

   We'll need the following files to run this program.
   - https://projecteuler.net/project/resources/p079_keylog.txt

   I saw the Wikipedia page about topological sorting.
   https://en.wikipedia.org/wiki/Topological_sorting

   Note: This implementation finds only one topological sort not all.
*)

open Core

exception Cycle of int list

let parse_data data =
  List.(
    map ~f:(Fun.compose (map ~f:Char.get_digit_exn) String.to_list) data
    |> concat_map ~f:(fun l ->
      [ (nth_exn l 0, nth_exn l 1)
      ; (nth_exn l 1, nth_exn l 2)
      ; (nth_exn l 0, nth_exn l 2)
      ])
    |> Euler.Util.list_assoc_group
    |> map ~f:(Tuple2.map_snd ~f:(dedup_and_sort ~compare:Int.ascending)))
;;

let dfs graph perm v =
  let rec visit temp visited node =
    List.(
      if mem temp node ~equal:Int.equal
      then raise (Cycle (rev (node :: temp)))
      else if mem visited node ~equal:Int.equal
      then visited
      else (
        match Assoc.find graph node ~equal:Int.equal with
        | Some dests -> node :: fold dests ~init:visited ~f:(visit (node :: temp))
        | None -> node :: []))
  in
  visit [] perm v
;;

let compute str_lst =
  let graph = parse_data str_lst in
  List.fold ~init:[] ~f:(fun perm (v, _) -> dfs graph perm v) graph
  |> Euler.Util.list_to_str Int.to_string ""
;;

let solve fname = compute (Euler.Task.read_file fname)

(* Test *)

let%test_unit "p079_keylog.txt" =
  [%test_eq: string]
    (compute (Euler.Task.read_file "./assets/p079_keylog.txt"))
    "73162890"
;;
