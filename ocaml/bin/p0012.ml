(* Project Euler: Problem 12 *)

let factors n =
  let u_limit = truncate(sqrt (float n)) in
  let rec aux lst i =
    if i > u_limit then
      lst
    else
      if n mod i <> 0 then
        aux lst (succ i)
      else
        if i * i = n then
          aux (i::lst) (succ i)
        else
          aux ((n / i) :: (i::lst)) (succ i)
  in
  aux [] 1
                          
let rec find_num a b =
  let tri_num = a + b in
  let divs = factors tri_num in
  if List.length divs >= 500 then
    tri_num
  else
    find_num tri_num (succ b)

let () =
  Printf.printf "the value of the first triangle number to have over five hundred divisors is %d\n" (find_num 0 1)
