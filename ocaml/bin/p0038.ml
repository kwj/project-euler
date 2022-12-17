(* Project Euler: Problem 38 *)

(*
  1) not more than five digits from condition.
  2) if number is four digits, n = 2  (x1 -> 4-digits, x2 -> 5-digits)
  3) if number is three digits, n = 3  (x1 -> 3-digits, x2 -> 3-digits, x3 -> 3-digits)
  4) if number is two digits, n = 4  (x1 -> 2-digits, x2 -> 2-digits, x3 -> 2-digits, x4 -> 3-digits)
  5) if number is own digit, n = 9 or 5 (only '1', '9').

  case #2:
    5000 <= x <= 9999
  case #3:
    100 <= x <= 333
  case #4:
    10 <= x <= 33
  case #5:
    x = 1, 9
 *)

(* ---------------------------------------------------------------- *)

let is_pandigital lst =
  let mk_bits num =
    let rec aux n bits =
      if n = 0 then bits else aux (n / 10) (bits lor (1 lsl ((n mod 10) - 1)))
    in
    aux num 0
  in
  let rec aux lst =
    match lst with
    | [] -> 0
    | hd :: tl -> (mk_bits hd) lor (aux tl)
  in
  aux lst = ((1 lsl 9) - 1)

let rec find_cands start stop =
  let mk_plist num =
    match num with
    | n when n = 1 -> [1; 2; 3; 4; 5; 6; 7; 8; 9]
    | n when n = 9 -> [9; 18; 27; 36; 45]
    | n when n < 100 -> [num; num * 2; num * 3; num * 4]
    | n when n < 1000 -> [num; num * 2; num * 3]
    | _ -> [num; num * 2]
  in
  if start < stop then find_cands stop start
  else
    let rec aux n result =
      if n < stop then result
      else
        let tmp = mk_plist n in
        if is_pandigital tmp = true then
          aux (pred n) ((int_of_string (List.fold_left (fun acc e -> acc ^ string_of_int e) "" tmp), n) :: result)
        else
          aux (pred n) result
    in
    aux start []

let solve () =
  let concat_products = List.sort (fun (p1, _) (p2, _) -> p2 - p1)
                          ((find_cands 9999 5000) @ (find_cands 333 100) @ (find_cands 33 10) @ (find_cands 9 9) @ (find_cands 1 1)) in
  List.hd concat_products

let () =
  let p, n = solve() in
  Printf.printf "Answer: %d (n=%d)\n" p n
