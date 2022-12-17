(* Project Euler: Problem 42 *)

(*
  We'll need the following file to run this program.
    - https://projecteuler.net/project/resources/p042_words.txt
 *)

open Core

(*
  let rec aux num result =
    if num < 0 then
      result
    else
      aux (num - 1) ((Char.of_int_exn (Char.to_int 'A' + num), num + 1) :: result)
  in
  aux 25 []
*)
let value_tbl = Hashtbl.of_alist_exn (module Char)
                  [('A', 1); ('B', 2); ('C', 3); ('D', 4); ('E', 5); ('F', 6); ('G', 7);
                   ('H', 8); ('I', 9); ('J', 10); ('K', 11); ('L', 12); ('M', 13); ('N', 14);
                   ('O', 15); ('P', 16); ('Q', 17); ('R', 18); ('S', 19); ('T', 20); ('U', 21);
                   ('V', 22); ('W', 23); ('X', 24); ('Y', 25); ('Z', 26)]

let calc_scores lst =
  let word_score word =
    let word_value s =
      let result = ref 0 in
      String.iter s ~f:(fun c -> result := !result + Hashtbl.find_exn value_tbl c);
      !result
    in
    word_value word
  in
  List.map lst ~f:word_score

let parse_data data =
  let line = List.hd_exn data in
  String.sub line ~pos:1 ~len:(String.length line - 2)
              |> Str.split (Str.regexp {|","|})

let exec data =
  let words = parse_data data in
  Int.to_string (List.length (List.filter (calc_scores words) ~f:Euler.Math.is_triangular))

let () = Euler.Task.run_with_data exec (Euler.Task.read_data ())
