(* Project Euler: Problem 35 *)

(*
  When 10 or more, the only numbers that can be used are 1, 3, 7, 9.
  The reason is that rotation number must be prime.
 *)

(* ---------------------------------------------------------------- *)

let is_prime num =
  let upper = truncate @@ sqrt @@ float num in
  let rec aux n k =
    if k < 2 then
      true
    else
      (n mod k <> 0) && (aux n (k - 1))
  in
  if num <= 1 then false else aux num upper

let is_circular num =
  let rec pow b e = if e <= 0 then 1 else b * pow b (e - 1) in
  let k = (String.length (string_of_int num)) - 1 in
  let d = pow 10 k in
  let rec aux num cnt result =
    if cnt = 0 || result = false then
      result
    else
      let next_num = (num mod 10) * d + (num / 10) in
      aux next_num (pred cnt) (is_prime next_num)
  in
  aux num k true

let solve num =
  let start_num = if num mod 2 = 0 then (pred num) else num in
  let rec aux n result =
    if n < 100 then result
    else
      match n with
      | n when n mod 5 = 0 || n mod 3 = 0
        -> aux (n - 2) result
      | n when is_prime n = false
        -> aux (n - 2) result
      | n when is_circular n = true
        -> aux (n - 2) (succ result)
      | _
        -> aux (n - 2) result
  in
  (aux start_num 0) + 13

let () =
  Printf.printf "Answer: %d\n" (solve 999_999)
