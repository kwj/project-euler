(* Project Euler: Problem 54 *)

(*
 *  We'll need the following file to run this program.
 *   - https://projecteuler.net/project/resources/p054_poker.txt
 *
 * card rank:
 *   2, 3, 4, 5, 6, 7, 8, 9, Ten(10), Jack(11), Queen(12), King(13), Ace(14)
 *
 * hand:
 *   0 - High Card: Highest value card.
 *   1 - One Pair: Two cards of the same value.
 *   2 - Two Pairs: Two different pairs.
 *   3 - Three of a Kind: Three cards of the same value.
 *   4 - Straight: All cards are consecutive values.
 *   5 - Flush: All cards of the same suit.
 *   6 - Full House: Three of a kind and a pair.
 *   7 - Four of a Kind: Four cards of the same value.
 *   8 - Straight Flush: All cards are consecutive values of same suit.
 *   9 - Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
 *
 * hand rank:
 *   [hand; val_1; val_2; ...]  val_# : rank detail
 *     example:
 *       8H 3D JS 6S 4C -> [0; 11; 8; 6; 4; 3])     - High Card [0; 11] @ kicker: [8; 6; 4; 3]
 *       9S 3C 9C 5S JS -> [1; 9; 11; 5; 3])        - One Pair [1; 9] @ kicker : [11; 5; 3]
 *       5C AD 5D AC 9C -> [2; 14; 5; 9])           - Two Pair [2; 14; 5] @ kicker : [9]
 *       3H 8S 7D 7H 7S -> [3; 7; 8; 3])            - Three of a Kind [3; 7] @ kicker : [8; 3]
 *       7H 5D 6S 8H 9H -> [4; 9; 8; 7; 6; 5])      - Straight [4; 9; 8; 7; 6; 5]
 *       2H 6H 7H QH JH -> [5; 12; 11; 7; 6; 2])    - Flush [5; 12; 11; 7; 6; 2]
 *       4D 8C 8S 4S 4H -> [6; 4; 8])               - Full House [6; 4; 8]
 *       3S 8H 3D 3H 3C -> [7; 3; 8])               - Four of a Kind [7; 3] @ kicker : [8]
 *       8C 6C 7C 5C 9C -> [8; 9; 8; 7; 6; 5])      - Straight Flush [8; 9; 8; 7; 6; 5]
 *       AH JH TH QH KH -> [9; 14; 13; 12; 11; 10]) - Royal Flush [9; 14; 13; 12; 11; 10]
 *)

open Core

let get_handrank cards =
  let hand_RF = 9
  and hand_SF = 8
  and hand_FK = 7
  and hand_FH = 6
  and hand_F = 5
  and hand_S = 4
  and hand_TK = 3
  and hand_TP = 2
  and hand_OP = 1
  and hand_HC = 0 in
  let rank_to_num ch =
    List.Assoc.find_exn
      [ ('2', 2)
      ; ('3', 3)
      ; ('4', 4)
      ; ('5', 5)
      ; ('6', 6)
      ; ('7', 7)
      ; ('8', 8)
      ; ('9', 9)
      ; ('T', 10)
      ; ('J', 11)
      ; ('Q', 12)
      ; ('K', 13)
      ; ('A', 14)
      ]
      ~equal:Char.equal
      ch
  in
  let get_hand_info lst =
    let rec aux cnt acc = function
      | x1 :: (x2 :: _ as xs) when x1 = x2 -> aux (succ cnt) acc xs
      | x1 :: (_ :: _ as xs) -> aux 1 ((cnt, x1) :: acc) xs
      | x :: [] -> List.rev ((cnt, x) :: acc)
      | [] -> failwith "not reached"
    in
    aux 1 [] lst
    |> List.stable_sort ~compare:(fun (c1, _) (c2, _) -> Int.descending c1 c2)
  in
  let get_detail h_info = snd (List.unzip h_info) in
  let is_straight l =
    List.equal ( = ) (List.range (List.hd_exn l) (List.hd_exn l - 5) ~stride:(-1)) l
  in

  let rank_lst, suit_lst = List.map ~f:(fun s -> (s.[0], s.[1])) cards |> List.unzip in
  let num_lst = List.map ~f:rank_to_num rank_lst |> List.sort ~compare:Int.descending in
  let hand_info = get_hand_info num_lst in

  let num_suits = List.dedup_and_sort ~compare:Char.compare suit_lst |> List.length in
  if num_suits = 1
  then (
    (* This hand contains five cards all of the same suit. *)
    match is_straight num_lst with
    | true ->
      if List.hd_exn num_lst = 14
      then hand_RF :: get_detail hand_info (* Royal Flush *)
      else hand_SF :: get_detail hand_info (* Straight Flush *)
    | false -> hand_F :: get_detail hand_info (* Flush *))
  else (
    match List.length hand_info with
    | 5 ->
      if is_straight num_lst
      then hand_S :: get_detail hand_info (* Straight *)
      else hand_HC :: get_detail hand_info (* High Card *)
    | 4 -> hand_OP :: get_detail hand_info (* One Pair *)
    | 3 ->
      let flag, _ = List.hd_exn hand_info in
      if flag = 3
      then hand_TK :: get_detail hand_info (* Three of a Kind *)
      else hand_TP :: get_detail hand_info (* Two Pair *)
    | 2 ->
      let flag, _ = List.hd_exn hand_info in
      if flag = 4
      then hand_FK :: get_detail hand_info (* Four of a Kind *)
      else hand_FH :: get_detail hand_info (* Full House *)
    | _ -> failwith "not reached")
;;

let compare_hands hands_lst =
  let determine h1 h2 = List.compare Int.compare (get_handrank h1) (get_handrank h2) in
  let rec aux p1 p2 draw = function
    | [] -> (p1, p2, draw)
    | (h1, h2) :: xs ->
      (match determine h1 h2 with
       | 1 -> aux (succ p1) p2 draw xs
       | -1 -> aux p1 (succ p2) draw xs
       | _ -> aux p1 p2 (succ draw) xs)
  in
  aux 0 0 0 hands_lst
;;

let parse_data data =
  List.map ~f:(fun s -> List.split_n (Str.split (Str.regexp " ") s) 5) data
;;

let compute str_lst =
  let p1, _p2, _draw = compare_hands (parse_data str_lst) in
  p1
;;

let solve fname = compute (Euler.Task.read_file fname) |> Int.to_string

(* Test *)

let%test_unit "p042_words.txt" =
  [%test_eq: int] (compute (Euler.Task.read_file "./assets/p054_poker.txt")) 376
;;
