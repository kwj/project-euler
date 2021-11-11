(* Project Euler: Problem 21 *)

let find_amicable num =
  let divisors n =
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
    List.filter (fun x -> x <> n) (aux [] 1)
  in
  let rec aux num result =
    if num < 2 then
      result
    else
      if List.mem num result then
        aux (pred num) result
      else
        let d_num = List.fold_left (+) 0 (divisors num) in
        let d_x = List.fold_left (+) 0 (divisors d_num) in
        if num = d_x && d_num <> d_x then
            aux (pred num) (d_num :: (d_x :: result))
        else
          aux (pred num) result
  in
  aux num []

let () =
  Printf.printf "The sum of all the amicable numbers under 10000 is %d\n" (List.fold_left (+) 0 (find_amicable 10000))
