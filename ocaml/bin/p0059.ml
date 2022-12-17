(* Project Euler: Problem 59 *)

(*
  ASCII
    reference: https://en.wikipedia.org/wiki/ASCII

    lower case characters:
      'a'..'z': 0x61(97d)..0x7a(122d)

  We'll need the following files to run this program.
    - https://projecteuler.net/project/resources/p059_cipher.txt   [encrypted data]
 *)

open Core

let keylist_generator len =
  let alpha = List.map ~f:(fun e -> [e]) (List.range ~stop:`inclusive 0x61 0x7a) in    (* 'a': 0x61, 'z': 0x7a *)
  let product_lst l1 l2 = List.concat (List.map ~f:(fun e -> List.map ~f:(fun x -> e @ x) l2) l1) in
  let keys = ref (product_lst alpha (product_lst alpha alpha)) in
  let make_keylist key =
    let rec aux cnt acc =
      if cnt = 0 then
        List.take (List.concat acc) len
      else
        aux (pred cnt) (key :: acc)
    in
    aux (len / 3 + 1) []
  in
  let next () =
    match !keys with
    | [] -> None
    | x :: xs -> keys := xs; Some (make_keylist x)
  in
  next

let decode cipher_text key =
  List.map2_exn cipher_text key ~f:(fun x y -> x lxor y)

let parse_data data =
  List.hd_exn data |> String.split ~on:',' |> List.map ~f:(fun s -> Int.of_string s)

let eval_ptext lst =
  let s_lst = List.map ~f:Char.of_int_exn lst |> String.of_char_list |> Str.split (Str.regexp "[,. ]") in
  let rec aux cnt = function
    | [] -> cnt
    | x :: xs -> aux (List.length (List.filter ~f:(String.(=) x) s_lst) + cnt) xs
  in
  aux 0 ["and"; "of"; "a"; "to"; "in"]

let solve data =
  let cipher_text = parse_data data in
  let get_key = keylist_generator (List.length cipher_text) in
  let rec aux max_score result =
    match get_key () with
    | None -> result
    | Some key -> let ptext_lst = decode cipher_text key in
                  let score = eval_ptext ptext_lst in
                  if max_score < score then
                    aux score key
                  else
                    aux max_score result
  in
  List.fold ~init:0 ~f:(+) (aux 0 [0] |> decode cipher_text)

let exec data =
  Int.to_string (solve data)

let () = Euler.Task.run_with_data exec (Euler.Task.read_data ())
