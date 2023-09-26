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

let parse_data data =
  List.hd_exn data |> String.split ~on:',' |> List.map ~f:(fun s -> Int.of_string s)
;;

let calc_score ch =
  match ch with
  | n when n = 0x20 -> 3 (* SPC *)
  | n when n >= 0x41 && n <= 0x5A -> 5 (* 'A'..='Z' *)
  | n when n >= 0x61 && n <= 0x7A -> 3 (* 'a'..='z' *)
  | n when n >= 0x21 && n <= 0x7E -> 1 (* other ASCII characters *)
  | _ -> 0
;;

let compute str_lst =
  let cipher_text = parse_data str_lst in
  let rec loop max_score ans = function
    | [] -> ans
    | x :: xs ->
      let key = List.to_array x in
      let plain_text =
        List.mapi cipher_text ~f:(fun idx ch -> Int.bit_xor ch key.(idx mod 3))
      in
      let score = List.fold plain_text ~init:0 ~f:(fun acc ch -> acc + calc_score ch) in
      if score > max_score
      then loop score (List.sum (module Int) ~f:Fn.id plain_text) xs
      else loop max_score ans xs
  in
  loop
    0
    0
    (Euler.Util.permutation_with_repetition 3 (List.range 0x61 0x7A ~stop:`inclusive))
;;

let solve () =
  compute (Euler.Task.read_data "./src/assets/p059_cipher.txt") |> Int.to_string
;;

(* Test *)

let%test_unit "p059_cipher.txt" =
  [%test_eq: int] (compute (Euler.Task.read_file "./assets/p059_cipher.txt")) 129448
;;
