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
  List.map ~f:(Str.split (Str.regexp "")) data
  |> List.map ~f:(fun l -> List.map l ~f:Int.of_string)
  |> List.map ~f:(fun l ->
    [ (List.nth_exn l 0, List.nth_exn l 1)
    ; (List.nth_exn l 1, List.nth_exn l 2)
    ; (List.nth_exn l 0, List.nth_exn l 2)
    ])
  |> List.concat
  |> Euler.Util.list_assoc_group
  |> List.map ~f:(fun (v, vs) -> (v, List.dedup_and_sort vs ~compare))
;;

let dfs graph perm v =
  let rec visit temp visited node =
    if List.mem temp node ~equal
    then raise (Cycle (List.rev (node :: temp)))
    else if List.mem visited node ~equal
    then visited
    else (
      match List.Assoc.find graph node ~equal with
      | Some dests ->
        node
        :: List.fold dests ~init:visited ~f:(fun perm v -> visit (node :: temp) perm v)
      | None -> node :: [])
  in
  visit [] perm v
;;

let compute str_lst =
  let graph = parse_data str_lst in
  List.fold ~init:[] ~f:(fun perm (v, _) -> dfs graph perm v) graph
  |> Euler.Util.list_to_str Int.to_string ""

let solve () = compute (Euler.Task.read_data "./src/assets/p079_keylog.txt")

(* Test *)

let%test_unit "p079_keylog.txt" =
  [%test_eq: string] (compute (Euler.Task.read_file "./assets/p079_keylog.txt")) "73162890"
