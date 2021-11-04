(* Project Euler: Problem 7 *)

let is_prime num =
  let upper = truncate @@ sqrt @@ float num in
  let rec aux n k =
    if k < 2 then
      true
    else
      (n mod k <> 0) && (aux n (k - 1))
  in
  if num <= 1 then false else aux num upper;;

let nth_prime num =
  let rec aux i n =
    if is_prime i = true then
      if n = 1 then i else aux (i + 1) (n - 1)
    else
      aux (i + 1) n
  in
  aux 2 num;;

let () =
  Printf.printf "the 10001st prime number is %d\n" (nth_prime 10001);;
