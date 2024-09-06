(* Project Euler: Problem 98 *)

open Core

let parse_data data =
  let line = List.hd_exn data in
  String.sub line ~pos:1 ~len:(String.length line - 2) |> Str.split (Str.regexp {|","|})
;;

let get_squares tbl n_digits =
  match Hashtbl.find tbl n_digits with
  | Some lst -> lst
  | None ->
    let lst =
      List.range
        (Euler.Math.isqrt (Int.pow 10 (n_digits - 1) - 1) + 1)
        (Euler.Math.isqrt (Int.pow 10 n_digits - 1))
        ~stop:`inclusive
      |> List.map ~f:(fun n -> Int.to_string (n * n))
    in
    Hashtbl.add_exn tbl ~key:n_digits ~data:lst;
    lst
;;

let get_trans_tbl w sq =
  let rec add_pairs tbl = function
    | [] -> ()
    | (k, v) :: xs ->
      Hashtbl.update tbl k ~f:(fun x ->
        match x with
        | None -> v
        | Some _ -> v);
      add_pairs tbl xs
  in
  let tmp_tbl = Hashtbl.create (module Char) in
  let trans_tbl = Hashtbl.create (module Char) in
  add_pairs tmp_tbl (List.zip_exn (String.to_list sq) (String.to_list w));
  Hashtbl.to_alist tmp_tbl |> List.map ~f:(fun (k, v) -> (v, k)) |> add_pairs trans_tbl;
  trans_tbl
;;

let get_max_anagram words sq_tbl =
  let squares = get_squares sq_tbl (String.length (List.hd_exn words)) in

  let trans trans_tbl s =
    String.to_list s
    |> List.map ~f:(fun ch ->
      match Hashtbl.find trans_tbl ch with
      | None -> ch
      | Some x -> x)
    |> String.of_list
  in

  let aux w1 w2 =
    let rec loop res = function
      | [] -> res
      | x :: xs ->
        let trans_tbl = get_trans_tbl w1 x in
        if String.(trans trans_tbl w1 <> x)
        then loop res xs
        else (
          let tmp = trans trans_tbl w2 in
          if List.exists squares ~f:(fun sq -> String.(sq = tmp))
          then loop (Int.max res (Int.max (Int.of_string x) (Int.of_string tmp))) xs
          else loop res xs)
    in
    loop 0 squares
  in

  let ans = ref 0 in
  List.iteri words ~f:(fun idx w1 ->
    List.iter (List.drop words (idx + 1)) ~f:(fun w2 -> ans := Int.max !ans (aux w1 w2)));
  !ans
;;

let compute str_lst =
  let word_tbl = Hashtbl.create (module String) in
  List.iter (parse_data str_lst) ~f:(fun s ->
    let key = String.to_list s |> List.sort ~compare:Char.compare |> String.of_list in
    Hashtbl.update word_tbl key ~f:(fun v ->
      match v with
      | None -> [ s ]
      | Some lst -> s :: lst));

  let sq_tbl = Hashtbl.create (module Int) in
  Hashtbl.data word_tbl
  |> List.filter ~f:(fun lst -> List.length lst > 1)
  |> List.map ~f:(fun lst -> get_max_anagram lst sq_tbl)
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn
;;

let solve fname = compute (Euler.Task.read_file fname) |> Int.to_string

(* Test *)

let%test_unit "p098_words.txt" =
  [%test_eq: int] (compute (Euler.Task.read_file "./assets/p098_words.txt")) 18769
;;
