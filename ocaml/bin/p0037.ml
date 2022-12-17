(* Project Euler: Problem 37 *)

(*
  candidate numbers: [2357][1379]*[37] (n >= 10)
 *)

(* ---------------------------------------------------------------- *)

let is_prime num =
  let upper = truncate @@ sqrt @@ float num in
  let rec aux n k =
    if k < 2 then true else (n mod k <> 0) && (aux n (k - 1))
  in
  if num <= 1 then false else aux num upper

let append_top nums lst =
  let ndigits num =
    let rec aux n d = if n = 0 then d else aux (n / 10) (succ d) in
    if num = 0 then 1 else aux num 0
  in
  let rec aux ns result =
    match ns with
    | [] -> result
    | hd :: tl -> aux tl (result @ List.map (fun e -> hd * (int_of_float (float_of_int 10 ** (float_of_int (ndigits e)))) + e) lst)
  in
  aux nums []

let create_truncl_prime lst =
  List.filter is_prime (append_top [1; 3; 7; 9] lst)

let create_truncr_prime lst =
  let rec is_truncr_prime num =
    if num = 0 then
      true
    else
      if is_prime num then
        is_truncr_prime (num / 10)
      else
        false
  in
  List.filter is_truncr_prime (append_top [2; 3; 5; 7] lst)

let solve () =
  let rec aux lst acc =
    if List.length acc >= 11 then
      (* the problem statement says there are only 11 of them. *)
      acc
    else
      aux (create_truncl_prime lst) (create_truncr_prime lst @ acc)
  in
  aux [3; 7] []

let () =
  Printf.printf "Answer: %d\n" (List.fold_left (+) 0 (solve ()))
