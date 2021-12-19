(* Project Euler: Problem 61 *)

(* ---------------------------------------------------------------- *)

(*
  P[n,r] = (map insert(x) P[n-1,r-1]) + P[n-1,r]
  :: 'x' is the first element of 'n'
  :: 'n-1' is the elements without x
 *)
let rec perm r lst =
  let rec insert x lst =
    match lst with
    | [] -> [[x]]
    | hd::tl -> (x::lst) :: (List.map (fun l -> hd::l) (insert x tl))
  in
  if r = 1 then
    List.map (fun e -> [e]) lst
  else
    match lst with
    | [] -> []
    | hd::tl -> List.append
                  (List.concat (List.map (fun e -> (insert hd e)) (perm (pred r) tl)))
                  (perm r tl)

let create_pdata () =
  let calc_p_list i =
    let calc_p i n =
      match i with
      | 3 -> n * (n + 1) / 2
      | 4 -> n * n
      | 5 -> n * (3 * n - 1) / 2
      | 6 -> n * (2 * n - 1)
      | 7 -> n * (5 * n - 3) / 2
      | 8 -> n * (3 * n - 2)
      | _ -> failwith "out of range"
    in
    let rec aux n acc =
      let p = calc_p i n in
      if p < 1000 then aux (succ n) acc
      else
        if p > 9999 then acc
        else
          aux (succ n) (p :: acc)
    in
    List.rev (aux 1 [])
  in
  let split_num n = [n / 100; n mod 100] in
  let rec aux lst acc =
    match lst with
    | [] -> acc
    | hd :: tl -> aux tl ((hd, List.map (split_num) (calc_p_list hd)) :: acc)
  in
  aux [3; 4; 5; 6; 7; 8] []

let find_cyclic patt pdata =
  let rec loop p acc =
    match p with
    | [] -> acc
    | hd :: tl ->
       loop tl (List.assoc hd pdata
                |> List.map (fun lst1 ->
                       List.map (fun l -> (List.nth lst1 0) :: l)
                         (List.filter (fun lst2 -> List.nth lst1 1 = List.nth lst2 0) acc))
                |> List.filter ((<>) [])
                |> List.flatten)
  in
  let restore_num lst =
    let nth_0 = List.hd lst in
    let rec aux l acc =
      match l with
      | [] -> acc
      | a :: (b :: _ as tl) -> aux tl ((100 * a + b) :: acc)
      | a :: [] -> aux [] ((100 * a + nth_0) :: acc)
    in
    aux lst []
  in
  let cands = loop (List.tl patt) (List.assoc (List.hd patt) pdata)
              |> List.filter ((<>) []) in
  if cands = [] then
    None
  else
    let rec aux c_lst acc =
      match c_lst with
      | [] ->
         if acc = [] then None else Some (List.map restore_num acc)
      | l :: ls ->
         if List.hd l = List.nth l (List.length l - 1) then
           aux ls ((List.tl l) :: acc)
         else
           aux ls acc
    in
    aux cands []

let solve () =
  let pdata = create_pdata() in
  let patt_lst = perm 6 [3; 4; 5; 6; 7; 8] in
  let rec uniq lst =
    match lst with
    | [] -> []
    | hd1 :: (hd2 :: _ as tl) when hd1 = hd2 -> uniq tl
    | hd :: tl -> hd :: (uniq tl)
  in
  let rec loop patts acc =
    match patts with
    | [] -> acc
    | hd :: tl ->
       match find_cyclic hd pdata with
       | None -> loop tl acc
       | Some v -> loop tl (v :: acc)
  in
  loop patt_lst []
  |> List.flatten
  |> List.map (List.sort compare)
  |> List.sort compare
  |> uniq
  |> List.flatten

let () =
  Printf.printf "Answer: %d\n" (List.fold_left (+) 0 (solve()))
