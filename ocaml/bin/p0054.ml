(* Project Euler: Problem 54 *)

(*
  rank:
    0 - High Card: Highest value card.
    1 - One Pair: Two cards of the same value.
    2 - Two Pairs: Two different pairs.
    3 - Three of a Kind: Three cards of the same value.
    4 - Straight: All cards are consecutive values.
    5 - Flush: All cards of the same suit.
    6 - Full House: Three of a kind and a pair.
    7 - Four of a Kind: Four cards of the same value.
    8 - Straight Flush: All cards are consecutive values of same suit.
    9 - Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.

  card:
    Ace(1, only if five-high straight), 2, 3, 4, 5, 6, 7, 8, 9, Ten(10), Jack(11), Queen(12), King(13), Ace(14)

  hand(record):
    (num of suits, [(value, num of value); ...], [(num of value, value); ...])   - lists are descending order
      example: 5C AD 5D AC 9C
                 -> (2, [(14, 2); (5, 2); (9, 1)], [(2, 14); (2, 5); (1, 9)])
             : 9S 3C 9C 5S JS
                 -> (2, [(11, 1); (9, 2); (5, 1); (3, 1)], [(2, 9); (1, 11); (1, 5); (1, 3)])
             : 7H 5D 6S 8H 9H
                 -> (3, [(9, 1); (8, 1); (7, 1); (6, 1); (5, 1)], [(1, 9); (1, 8); (1, 7); (1, 6); (1, 5)])
             : 2D AS 5H 3H 4D
                 -> (3, [(14, 1); (5, 1); (4, 1); (3, 1); (2, 1)], [(1, 14); (1, 5); (1, 4); (1, 3); (1, 2)])
  result(score):
    [rank; weight; ...]]
      example: (2, [(14, 2); (5, 2); (9, 1)], [(2, 14); (2, 5); (1, 9)])
                 -> [2; 14; 5; 9]
               (2, [(11, 1); (9, 2); (5, 1); (3, 1)], [(2, 9); (1, 11); (1, 5); (1, 3)])
                 -> [1; 9; 11; 5; 3]
               (3, [(9, 1); (8, 1); (7, 1); (6, 1); (5, 1)], [(1, 9); (1, 8); (1, 7); (1, 6); (1, 5)])
                 -> [4; 9; 8; 7; 6; 5]
               (3, [(14, 1); (5, 1); (4, 1); (3, 1); (2, 1)], [(1, 14); (1, 5); (1, 4); (1, 3); (1, 2)])
                 -> [4; 5; 4; 3; 2; 1]
 *)

(* ---------------------------------------------------------------- *)

let read_data() =
  let filename = ref "" in
  let anon_fun n = () in
  let speclist = [("-f", Arg.Set_string filename, "<filename>  Set input file name (If not specified, read from stdin)")] in
  Arg.parse speclist anon_fun "Usage:";
  if !filename <> "" then
    let fin = open_in !filename in
    let rec loop acc =
      match input_line fin with
      | l -> loop (l :: acc)
      | exception End_of_file -> close_in fin; List.rev acc
    in
    loop []
  else
    let rec loop acc =
      match read_line () with
      | l -> loop (l :: acc)
      | exception End_of_file -> List.rev acc
    in
    loop []

let make_records hands =
  let make_ninfo nums =
    let card_tbl = Hashtbl.create 5 in
    let rec count_up = function
      | hd :: tl -> if Hashtbl.mem card_tbl hd then (
                      Hashtbl.replace card_tbl hd ((Hashtbl.find card_tbl hd) + 1)
                    ) else (
                      Hashtbl.add card_tbl hd 1
                    );
                    count_up tl
      | [] -> card_tbl
    in
    let c_to_val c =
      List.assoc c [('2', 2); ('3', 3); ('4', 4); ('5', 5); ('6', 6); ('7', 7); ('8', 8);
                    ('9', 9); ('T', 10); ('J', 11); ('Q', 12); ('K', 13); ('A', 14)]
    in
    let nk_card, ck_card = count_up nums
                           |> Hashtbl.to_seq
                           |> Seq.map (fun (c, cnt) -> (((c_to_val c), cnt), (cnt, (c_to_val c))))
                           |> List.of_seq
                           |> List.split in
    let cmp_tpl (m1, n1) (m2, n2) = if m1 <> m2 then m2 - m1 else n2 - n1 in
    List.sort cmp_tpl nk_card, List.sort cmp_tpl ck_card
  in
  let make_sinfo suits =
    let rec aux = function
      | a :: (b :: _ as tl) when a = b -> aux tl
      | a :: b -> a :: aux b
      | [] -> []
    in
    List.length (aux (List.sort compare suits))
  in
  let make_handinfo lst =
    List.map (fun cards -> List.map (fun s -> (s.[0], s.[1])) cards) lst
    |> List.map (List.split)
    |> List.map (fun (nums, suits) -> let vn, nv = make_ninfo nums in (make_sinfo suits, vn, nv))
  in
  make_handinfo hands

let cnvt_hands data =
  List.map (Str.split (Str.regexp " ")) data
  |> List.map (function
         | c1 :: c2 :: c3 :: c4 :: c5 :: tl -> ([c1; c2; c3; c4; c5], tl)
         | _ -> assert false)
  |> List.split

let calc_score (nsuits, vn_lst, nv_lst) =
  let is_straight vn_lst =
    let v, _ = List.split vn_lst in
    match v with
    | v1 :: v2 :: v3 :: v4 :: v5 :: [] when v1 - v2 = 1 && v2 - v3 = 1 && v3 - v4 = 1 && v4 - v5 = 1
      -> Some v
    | v1 :: v2 :: v3 :: v4 :: v5 :: [] when v1 = 14 && v2 = 5 && v3 = 4 && v4 = 3 && v5 = 2
      -> Some [5; 4; 3; 2; 1]      (* five-high straight *)
    | _ -> None
  in
  let other_rank nv_lst =
    let _, v = List.split nv_lst in
    match List.length nv_lst with
    | 2 -> if List.mem_assoc 4 nv_lst then 7 :: v else 6 :: v      (* 7: four of a kind, 6: full house *)
    | 3 -> if List.mem_assoc 3 nv_lst then 3 :: v else 2 :: v      (* 3: three of a kind, 2: two pairs *)
    | 4 -> 1 :: v      (* 1: one pair *)
    | _ -> 0 :: v      (* 0: high card *)
  in
  if nsuits = 1 then
    match is_straight vn_lst with
    | Some v -> if List.hd v = 14 then 9 :: v else 8 :: v      (* 9: royal flush, 8: straight flush *)
    | None -> let v, _ = List.split vn_lst in 5 :: v      (* 5: flush *)
  else
    match is_straight vn_lst with
    | Some v -> 4 :: v      (* 4: straight *)
    | None -> other_rank nv_lst

let judgement recs =
  let rec aux r_lst w1 w2 d =
    match r_lst with
    | (p1, p2) :: tl when p1 = p2 -> aux tl w1 w2 (succ d)      (* draw *)
    | (p1, p2) :: tl when p1 > p2 -> aux tl (succ w1) w2 d      (* player 1 win *)
    | (p1, p2) :: tl when p1 < p2 -> aux tl w1 (succ w2) d      (* player 2 win *)
    | _ -> w1, w2, d
  in
  aux recs 0 0 0

let solve () =
  let p1_hands, p2_hands = read_data () |> cnvt_hands in
  judgement (List.combine (List.map calc_score (make_records p1_hands)) (List.map calc_score (make_records p2_hands)))

let () =
  let p1, p2, d = solve () in
  Printf.printf "Answer:\n  Player1 win = %d\n  Player2 win = %d\n  draw = %d\n" p1 p2 d
