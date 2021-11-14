(* Project Euler: Problem 24 *)

let rec fact n = if n = 0 then 1 else n * fact (n - 1)

let find_elem lst r idx =
  let make_fact_table num r =
    let rec aux n cnt result =
      if cnt <= 0 then
        List.rev result
      else
        aux (n - 1) (cnt - 1) ((fact (n - 1) / fact (num - r)) :: result)
    in
    aux num r []
  in
  let rec aux f_lst elem_lst result i =
    match f_lst with
    | hd :: tl ->
       let n = List.nth elem_lst (i / hd) in
       aux tl (List.filter ((<>) n) elem_lst) (n :: result) (i mod hd)
    | [] ->
       List.rev result
  in
  if idx < 0 || idx >= fact(List.length lst) / fact(List.length lst - r) then
    failwith "out of range"
  else
    aux (make_fact_table (List.length lst) r) lst [] idx

let digits_to_str lst =
  List.fold_left (fun i elem -> i ^ string_of_int elem) "" lst

let solve nth =
  digits_to_str (find_elem [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] 10 (pred nth))

let () =
  Printf.printf "the millionth lexicographic permutation of \
                 the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9 is %s\n" (solve 1_000_000)
