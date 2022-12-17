
let rec permutation n lst =
  let rec insert_elm elm lst =
    match lst with
    | [] -> [[elm]]
    | x :: xs -> (elm :: lst) :: (List.map (fun l -> x :: l) (insert_elm elm xs))
  in
  if n = 1 then
    List.map (fun elm -> [elm]) lst
  else
    match lst with
    | [] -> []
    | x :: xs -> List.append
                   (List.concat (List.map (fun e -> insert_elm x e) (permutation (pred n) xs)))
                   (permutation n xs)

let rec combination n lst =
  match n, lst with
  | 0, _ -> [[]]
  | _, []  -> []
  | num, x :: xs -> List.map (List.cons x) (combination (num - 1) xs) @ combination num xs

(* example: [1;2;3] -> 123 *)
let list_to_num lst =
  let rec aux lst result =
    match lst with
    | [] -> result
    | x :: xs -> aux xs (result * 10 + x)
  in
  aux lst 0

let num_of_list = list_to_num

(* num >= 0; example 123 -> [1;2;3] *)
let num_to_list num =
  let rec aux n result =
    if n = 0 then
      result
    else
      aux (n / 10) ((n mod 10) :: result)
  in
  match aux num [] with
  | [] -> [0]
  | n -> n

let list_of_num = num_to_list

(* int_to_nlst 123 -> [3; 2; 1] *)
let int_to_nlst num =
  List.rev (num_to_list num)

let nlst_of_int = int_to_nlst

(* nlst_to_int [3; 2; 1] -> 123 *)
let nlst_to_int lst =
  list_to_num (List.rev lst)

let int_of_nlst = nlst_to_int

(*
   cmp_nlst [2; 1] [0; 0; 1] -> -1
   cmp_nlst [2; 1; 1] [0; 0; 1] -> 1
   cmp_nlst [2; 1; 1] [2; 1; 1] -> 0
 *)
let rec cmp_nlst l1 l2 =
  let diff = (List.length l1) - (List.length l2) in
  if diff = 0 then
    List.compare (fun e1 e2 -> e1 - e2) (List.rev l1) (List.rev l2)
  else
    if diff > 0 then
      cmp_nlst l1 (l2 @ [0])
    else
      cmp_nlst (l1 @ [0]) l2

(* add_nlst [8; 2; 1] [6; 5; 2] -> [4; 8; 3] *)
let rec add_nlst l1 l2 =
  let rec aux l1 l2 carry acc =
    match l1, l2 with
    | x :: xs, y :: ys ->
       aux xs ys ((x + y + carry) / 10) (((x + y + carry) mod 10) :: acc)
    | [], [] when carry > 0 ->
       List.rev (carry :: acc)
    | _ ->
       List.rev acc
  in
  let diff = (List.length l1) - (List.length l2) in
  if diff = 0 then
    aux l1 l2 0 []
  else
    if diff > 0 then
      add_nlst l1 (l2 @ [0])
    else
      add_nlst (l1 @ [0]) l2

(* sub_nlst [4; 8; 3] [6; 5; 2] -> [8; 2; 1] *)
let rec sub_nlst l1 l2 =
  let trim_zero lst =
    let rec loop l =
      if List.hd l = 0 then
        loop (List.tl l)
      else
        List.rev l
    in
    loop (List.rev lst)
  in
  (* List.rev l1 > List.rev l2 *)
  let rec aux l1 l2 carry acc =
    match l1, l2 with
    | x :: xs, y :: ys ->
       let tmp = x - y - carry in
       if tmp < 0 then
         aux xs ys (((abs tmp) + 9) / 10) ((abs ((tmp + 10) mod 10)) :: acc)
       else
         aux xs ys 0 ((tmp mod 10) :: acc)
    | [], [] when carry > 0 ->
       trim_zero (List.rev (carry :: acc))
    | _ ->
       trim_zero (List.rev acc)
    in
    let diff = (List.length l1) - (List.length l2) in
    if diff = 0 then
      if cmp_nlst l1 l2 >= 0 then
        aux l1 l2 0 []
      else
        aux l2 l1 0 []
    else
      if diff > 0 then
        sub_nlst l1 (l2 @ [0])
      else
        sub_nlst (l1 @ [0]) l2

(* mul_nlst [1; 4; 2] 3 -> [3; 2; 7] *)
let mul_nlst lst num =
  let rec aux lst carry acc =
    match lst with
    | x :: xs ->
       aux xs ((x * num + carry) / 10) (((x * num + carry) mod 10) :: acc)
    | [] when carry > 0 ->
       aux [] (carry / 10) ((carry mod 10) :: acc)
    | _ ->
       List.rev acc
  in
  aux lst 0 []

(* trim_zero_nlst [1; 2; 3; 0; 0] -> [1; 2; 3] *)
let trim_zero_nlst lst =
  let rec loop l =
    if List.hd l = 0 then
      loop (List.tl l)
    else
      List.rev l
  in
  loop (List.rev lst)

(* https://en.wikipedia.org/wiki/Hamming_weight *)
let popcount_64 n =
  let open Int64 in
  let ( land ) = logand and ( lsr ) = shift_right_logical and
      ( + ) = add and ( - ) = sub and ( * ) = mul and
      m1 = 0x5555555555555555L and m2 = 0x3333333333333333L and
      m4 = 0x0f0f0f0f0f0f0f0fL and h01 = 0x0101010101010101L in
  let x = n - ((n lsr 1) land m1) in
  let x = (x land m2) + ((x lsr 2) land m2) in
  let x = (x + (x lsr 4)) land m4 in
  to_int ((x * h01) lsr 56)

let popcount_32 n =
  popcount_64 Int64.(logand (of_int32 n) 0xffffffffL)

let mask_int =
  let rec aux cnt result =
    if cnt = 0 then
      result
    else
      aux (pred cnt) Int64.(logor (shift_left result 1) one)
  in
  aux (Sys.int_size - 2) Int64.one

let popcount_int n =
  popcount_64 Int64.(logand (of_int n) mask_int)

let popcount = popcount_int

let popcount_char c =
  popcount_int (Char.code c)

let popcount_nativeint n =
  match Nativeint.size with
    32 -> popcount_32 (Nativeint.to_int32 n)
  | 64 -> popcount_64 (Int64.of_nativeint n)
  | _ -> assert false


(*
  Number of Trailing Zero (NTZ), aka Count Trailing Zero (CTZ)

  let m_seq64 = 0x03F566ED27179461L
  let arr = Array.make 64 0 in
  let rec loop m i =
    if i = 64 then
      arr
    else (
      arr.(Int64.(to_int (shift_right_logical m 58))) <- i;
      loop (Int64.shift_left m 1) (succ i)
    )
  let tbl = loop m_seq64 0
*)

let get_NTZ_64 n =
  let open Int64 in
  let ( land ) = logand and ( lsr ) = shift_right_logical and
      ( = ) = equal and ( * ) = mul in
  let m_seq64 = 0x03F566ED27179461L and
      tbl = [|0; 1; 59; 2; 60; 40; 54; 3; 61; 32; 49; 41; 55; 19; 35; 4; 62; 52; 30; 33;
              50; 12; 14; 42; 56; 16; 27; 20; 36; 23; 44; 5; 63; 58; 39; 53; 31; 48; 18;
              34; 51; 29; 11; 13; 15; 26; 22; 43; 57; 38; 47; 17; 28; 10; 25; 21; 37; 46;
              9; 24; 45; 8; 7; 6|] in
  if n = 0L then
    64
  else
    let x = n land (neg n) in
    tbl.(to_int ((x * m_seq64) lsr 58))

let get_NTZ_32 n =
  get_NTZ_64 Int64.(logand (of_int32 n) 0xffffffffL)

let get_NTZ_int n =
  get_NTZ_64 Int64.(logand (of_int n) mask_int)

let get_NTZ = get_NTZ_int

let get_NTZ_char c =
  get_NTZ_int (Char.code c)

let get_NTZ_nativeint n =
  match Nativeint.size with
    32 -> get_NTZ_32 (Nativeint.to_int32 n)
  | 64 -> get_NTZ_64 (Int64.of_nativeint n)
  | _ -> assert false

let list_to_str fn gap lst =
  match List.length lst with
    0 -> ""
  | 1 -> fn (List.hd lst)
  | _ -> (fn (List.hd lst)) ^ List.fold_left (fun acc elt -> acc ^ gap ^ (fn elt)) "" (List.tl lst)

let list_assoc_group tpl_lst =
  let rec group key acc lst result =
    match lst with
    | x :: xs -> if key = fst x then
                   group key ((snd x) :: acc) xs result
                 else
                   group (fst x) [snd x] xs ((key, acc) :: result)
    | [] -> List.rev ((key, acc) :: result)
  in
  let sorted_lst = List.sort (fun (k1, _) (k2, _) -> compare k1 k2) tpl_lst in
  let init_key, init_value = List.hd sorted_lst in
  group init_key [init_value] (List.tl sorted_lst) []

  
