let rec choose left remaining =
  match remaining with
  | x :: xs -> (x, List.rev left @ xs) :: choose (x :: left) xs
  | [] -> []
;;

let choose lst = choose [] lst

let rec permutation n lst =
  match (n, lst) with
  | 0, _ -> [ [] ]
  | _, [] -> []
  | n, lst ->
    List.concat
      (List.map
         (fun (x, xs) -> List.map (List.cons x) (permutation (n - 1) xs))
         (choose lst))
;;

let rec permutation_with_repetition n lst =
  match (n, lst) with
  | 0, _ -> [ [] ]
  | _, [] -> []
  | n, lst ->
    List.concat
      (List.map
         (fun (x, _) -> List.map (List.cons x) (permutation_with_repetition (n - 1) lst))
         (choose lst))
;;

let rec combination n lst =
  match (n, lst) with
  | 0, _ -> [ [] ]
  | _, [] -> []
  | num, x :: xs -> List.map (List.cons x) (combination (num - 1) xs) @ combination num xs
;;

let rec combination_with_repetition n lst =
  match (n, lst) with
  | 0, _ -> [ [] ]
  | _, [] -> []
  | num, x :: xs ->
    List.map (List.cons x) (combination_with_repetition (num - 1) lst)
    @ combination_with_repetition num xs
;;

let rec powerset = function
  | [] -> [ [] ]
  | x :: xs ->
    let llst = powerset xs in
    llst @ List.map (fun lst -> x :: lst) llst
;;

let findall f lst =
  List.mapi (fun idx x -> (idx, x)) lst
  |> List.filter (fun (_, x) -> f x)
  |> List.map (fun (idx, _) -> idx)
;;

(* https://en.wikipedia.org/wiki/Hamming_weight *)
let popcount_64 n =
  let open Int64 in
  let ( land ) = logand
  and ( lsr ) = shift_right_logical
  and ( + ) = add
  and ( - ) = sub
  and ( * ) = mul
  and m1 = 0x5555555555555555L
  and m2 = 0x3333333333333333L
  and m4 = 0x0f0f0f0f0f0f0f0fL
  and h01 = 0x0101010101010101L in
  let x = n - ((n lsr 1) land m1) in
  let x = (x land m2) + ((x lsr 2) land m2) in
  let x = (x + (x lsr 4)) land m4 in
  to_int ((x * h01) lsr 56)
;;

let popcount_32 n = popcount_64 Int64.(logand (of_int32 n) 0xffffffffL)

let mask_int =
  let rec aux cnt result =
    if cnt = 0 then result else aux (pred cnt) Int64.(logor (shift_left result 1) one)
  in
  aux (Sys.int_size - 2) Int64.one
;;

let popcount_int n = popcount_64 Int64.(logand (of_int n) mask_int)
let popcount = popcount_int
let popcount_char c = popcount_int (Char.code c)

let popcount_nativeint n =
  match Nativeint.size with
  | 32 -> popcount_32 (Nativeint.to_int32 n)
  | 64 -> popcount_64 (Int64.of_nativeint n)
  | _ -> assert false
;;

(*
 * Number of Trailing Zero (NTZ), aka Count Trailing Zero (CTZ)
 *
 * let m_seq64 = 0x03F566ED27179461L
 * let arr = Array.make 64 0 in
 * let rec loop m i =
 *   if i = 64 then
 *     arr
 *   else (
 *     arr.(Int64.(to_int (shift_right_logical m 58))) <- i;
 *     loop (Int64.shift_left m 1) (succ i)
 *   )
 * let tbl = loop m_seq64 0
 *)
let get_NTZ_64 n =
  let open Int64 in
  let ( land ) = logand
  and ( lsr ) = shift_right_logical
  and ( = ) = equal
  and ( * ) = mul in
  let m_seq64 = 0x03F566ED27179461L
  and tbl =
    [| 0; 1; 59; 2; 60; 40; 54; 3; 61; 32; 49; 41; 55; 19; 35; 4; 62; 52; 30; 33;
       50; 12; 14; 42; 56; 16; 27; 20; 36; 23; 44; 5; 63; 58; 39; 53; 31; 48; 18;
       34; 51; 29; 11; 13; 15; 26; 22; 43; 57; 38; 47; 17; 28; 10; 25; 21; 37; 46;
       9; 24; 45; 8; 7; 6
    |] [@ocamlformat "disable"]
  in
  if n = 0L
  then 64
  else (
    let x = n land neg n in
    tbl.(to_int ((x * m_seq64) lsr 58)))
;;

let get_NTZ_32 n = get_NTZ_64 Int64.(logand (of_int32 n) 0xffffffffL)
let get_NTZ_int n = get_NTZ_64 Int64.(logand (of_int n) mask_int)
let get_NTZ = get_NTZ_int
let get_NTZ_char c = get_NTZ_int (Char.code c)

let get_NTZ_nativeint n =
  match Nativeint.size with
  | 32 -> get_NTZ_32 (Nativeint.to_int32 n)
  | 64 -> get_NTZ_64 (Int64.of_nativeint n)
  | _ -> assert false
;;

let list_to_str fn gap lst =
  match List.length lst with
  | 0 -> ""
  | 1 -> fn (List.hd lst)
  | _ ->
    fn (List.hd lst) ^ List.fold_left (fun acc elt -> acc ^ gap ^ fn elt) "" (List.tl lst)
;;

let list_assoc_group tpl_lst =
  let rec group key acc lst result =
    match lst with
    | x :: xs ->
      if key = fst x
      then group key (snd x :: acc) xs result
      else group (fst x) [ snd x ] xs ((key, acc) :: result)
    | [] -> List.rev ((key, acc) :: result)
  in
  let sorted_lst = List.sort (fun (k1, _) (k2, _) -> compare k1 k2) tpl_lst in
  let init_key, init_value = List.hd sorted_lst in
  group init_key [ init_value ] (List.tl sorted_lst) []
;;

let digits ?(base = 10) n =
  let rec aux n lst =
    if n = 0 then List.rev lst else aux (n / base) ((n mod base) :: lst)
  in
  aux n []
;;

let undigits ?(base = 10) lst =
  List.rev lst |> List.fold_left (fun acc x -> (acc * base) + x) 0
;;

let z_digits ?(base = 10) n =
  let rec aux n lst =
    if n = Z.zero
    then List.rev lst
    else aux Z.(div n ~$base) (Z.(to_int (rem n ~$base)) :: lst)
  in
  aux n []
;;

let z_undigits ?(base = 10) lst =
  List.rev lst |> List.fold_left (fun acc x -> Z.(add (mul acc ~$base) ~$x)) Z.zero
;;
