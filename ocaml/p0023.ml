(* Project Euler: Problem 23 *)

let const_upper = 28123

let find_abundant_numbers upper =
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
    if num < 12 then  (* 12: minimum abundant number *)
      result
    else
      if List.fold_left (+) 0 (divisors num) > num then
        aux (pred num) (num :: result)
      else
        aux (pred num) result
  in
  aux upper []

let sum_of_targets an_lst =
  let sum = ref 0 in
  let tgt_table = Array.make (const_upper + 1) false in
  let an_str = Array.of_list an_lst in
  for i = 0 to (Array.length an_str) - 1 do
    for j = i to (Array.length an_str) - 1 do
      let idx = an_str.(i) + an_str.(j) in
      if idx <= const_upper then
        tgt_table.(idx) <- true
    done
  done;
  for i = 1 to (Array.length tgt_table) - 1 do
    if tgt_table.(i) = false then
      sum := !sum + i
  done;
  !sum

let () =
  Printf.printf "the sum of all the positive integers which cannot be \
                 written as the sum of two abundant numberr is %d\n"
    (sum_of_targets @@ find_abundant_numbers const_upper)
