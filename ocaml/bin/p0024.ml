(* Project Euler: Problem 24 *)

open Core

let rec fact n =
  if n = 0 then 1 else n * fact (n - 1)

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
       let n = List.nth_exn elem_lst (i / hd) in
       aux tl (List.filter elem_lst ~f:((<>) n)) (n :: result) (i mod hd)
    | [] ->
       List.rev result
  in
  if idx < 0 || idx >= fact (List.length lst) / fact (List.length lst - r) then
    failwith "out of range"
  else
    aux (make_fact_table (List.length lst) r) lst [] idx

let digits_to_str lst =
  List.fold lst ~f:(fun i elem -> i ^ string_of_int elem) ~init:""

let exec () =
  digits_to_str (find_elem [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] 10 (pred 1_000_000))

let () = Euler.Task.run exec
