(* Project Euler: Problem 98 *)

(*
  We will need the following file to run this program.
    - https://projecteuler.net/project/resources/p098_words.txt
 *)

open Core

module U = Euler.Util
module M = Euler.Math

(*
  -> (int * string list list) list
    example: [(2, [["O"; "N"]; ["N"; "O"]]); ...]
*)
let parse_data data =
  let line = List.hd_exn data in
  Str.split (Str.regexp {|","|}) (String.sub line ~pos:1 ~len:(String.length line - 2))
  |> List.map ~f:(fun s -> (List.fold ~init:"" ~f:(^)
                             (List.sort ~compare:String.compare (Str.split (Str.regexp "") s)),
                           Str.split (Str.regexp "") s))
  |> List.sort ~compare:(fun (k1, _) (k2, _) -> String.compare k1 k2)
  |> U.list_assoc_group
  |> List.filter ~f:(fun (_, kw_lst) -> List.length kw_lst > 1)
  |> List.map ~f:(fun (k, word_lst) -> List.map ~f:(fun l -> (String.length k, l)) (U.combination 2 word_lst))
  |> List.concat
  |> List.sort ~compare:(fun (k1, _) (k2, _) -> Int.compare k2 k1)

(*
  -> (int * int list list) list
    example: [(1, [[1]; [4]; [9]]); [(2, [[1; 6]; [2; 5]; [3; 6]; [4; 9]; [6; 4]; [8; 1]]); ... ]
*)
let make_sq_tbl wp_lst =
  let limit = M.isqrt (Int.pow 10 (fst (List.hd_exn wp_lst)) - 1) in
  let rec loop n acc =
    if n > limit then
      List.sort ~compare:(fun (k1, _) (k2, _) -> Int.compare k1 k2) acc
      |> U.list_assoc_group
    else
      let sq = n * n in
      loop (succ n) ((M.num_of_digits sq, U.num_to_list sq) :: acc)
  in
  loop 1 []

(*
  --> int list list option
    example: Some [[9216; 1296]; ...]
*)
let find_squares kw_pair sq_tbl =
  let kw1 = List.nth_exn (snd kw_pair) 0 and
      kw2 = List.nth_exn (snd kw_pair) 1 in

  let make_trans_tbl kw sq =
    let is_injective_ks match_lst =
      List.sort ~compare:(fun (k1, _) (k2, _) -> String.compare k1 k2) match_lst
      |> U.list_assoc_group
      |> List.for_all ~f:(fun (_, v) -> List.length (List.dedup_and_sort ~compare v) = 1)
    in
    let is_injective_sk match_lst =
      List.sort ~compare:(fun (k1, _) (k2, _) -> Int.compare k1 k2) match_lst
      |> U.list_assoc_group
      |> List.for_all ~f:(fun (_, v) -> List.length (List.dedup_and_sort ~compare:String.compare v) = 1)
    in
    let tbl_ks = List.zip_exn kw sq and
        tbl_sk = List.zip_exn sq kw in
    match is_injective_ks tbl_ks && is_injective_sk tbl_sk with
      true -> Some tbl_ks
    | false -> None
  in

  let kw_to_num kw tbl =
    List.map ~f:(fun s -> List.Assoc.find_exn tbl s ~equal:String.equal) kw
  in

  let rec loop acc = function
      [] -> if List.length acc = 0 then None else Some acc
    | sq :: sqs ->
        match make_trans_tbl kw1 sq with
          None -> loop acc sqs
        | Some tbl -> let tmp = kw_to_num kw2 tbl in
                      if (List.hd_exn tmp) <> 0 && (M.is_square (U.list_to_num tmp)) then
                        loop ([U.list_to_num sq; U.list_to_num tmp] :: acc) sqs
                      else
                        loop acc sqs
  in
  loop [] (List.Assoc.find_exn sq_tbl (fst kw_pair) ~equal)

let solve kw_pair_lst =
  let sq_tbl = make_sq_tbl kw_pair_lst in
  let rec loop acc lst =
    match lst with
      [] -> List.sort (List.concat acc) ~compare:(fun x y -> Int.compare y x)
    | x :: xs ->
        match find_squares x sq_tbl with
          None -> loop acc xs
        | Some lst -> loop ((List.concat lst) :: acc) xs
  in
  loop [] kw_pair_lst

let exec data =
  Int.to_string (List.hd_exn (solve (parse_data data)))

let () = Euler.Task.run_with_data exec (Euler.Task.read_data ())
