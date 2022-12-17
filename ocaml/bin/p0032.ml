(* Project Euler: Problem 32 *)

(*
  m * n = mn (multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital)

  - numbers of digits of multiplicand/multiplier must be 4 or less.
  - if number of digits of multiplicand is 4, number of digits of multiplier is 1.
  - if number of digits of multiplicand is 3, number of digits of multiplier is 2.
 *)

(* ---------------------------------------------------------------- *)

let rec uniq lst =
  match lst with
  | [] -> []
  | hd1 :: (hd2 :: _ as tl) when hd1 = hd2 -> uniq tl
  | hd :: tl -> hd :: (uniq tl)

(*
  P[n,r] = (map insert(x) P[n-1,r-1]) + P[n-1,r]
  :: 'x' is the first element of 'n'
  :: 'n-1' is the elements without x
 *)
let rec permutation r lst =
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
                  (List.concat (List.map (fun e -> (insert hd e)) (permutation (pred r) tl)))
                  (permutation r tl)

let solve () =
  let const_numbers = List.init 9 (fun e -> e + 1) in
  let mk_tupl factor_lst ndigits =
    let rec rm_elems elms lst =
      match elms with
      | [] -> lst
      | hd :: tl -> rm_elems tl (List.filter (fun e -> e <> hd) lst)
    in
    let rec aux f_lst result =
      match f_lst with
      | [] -> List.flatten result
      | hd :: tl -> aux tl (List.map (fun e -> (hd, e)) (permutation ndigits (rm_elems hd const_numbers)) :: result)
    in
    aux factor_lst []
  in
  let l_to_num lst =
    let rec aux lst result =
      match lst with
      | [] -> result
      | hd ::tl -> aux tl (10 * result + hd)
    in
    aux lst 0
  in
  let is_pandigital (m1, m2, p) =
    let mk_bits num =
      let rec aux n bits =
        if n = 0 then bits else aux (n / 10) (bits lor (1 lsl ((n mod 10) - 1)))
      in
      aux num 0
    in
    (mk_bits m1) lor (mk_bits m2) lor (mk_bits p) = ((1 lsl 9) - 1)
  in
  List.append
    (mk_tupl (List.map (fun e -> [e]) (List.tl const_numbers)) 4)    (* multiplier: 1-digit / multiplicand: 4-digits *)
    (mk_tupl (permutation 2 const_numbers) 3)    (* multiplier: 2-digits / multiplicand: 3-digits *)
  |> List.map (fun (m1, m2) -> (l_to_num m1, l_to_num m2, (l_to_num m1) * (l_to_num m2)))
  |> List.filter (fun (_, _, p) -> (p >= 1000 && p < 10000))    (* product is 4-digits *)
  |> List.filter is_pandigital
  |> List.map (fun (_, _, p) -> p)
  |> List.sort compare
  |> uniq
  |> List.fold_left (+) 0

let () =
  Printf.printf "the sum of all products whose multiplicand/multiplier/product \
                 identity can be written as a 1 through 9 pandigital is %d\n" (solve())

