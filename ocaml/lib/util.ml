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
