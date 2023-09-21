(* Project Euler: Problem 24 *)

open Core

let rec fact n = if n = 0 then 1 else n * fact (n - 1)

let make_factorial_tbl num r =
  let rec aux n cnt lst =
    if cnt = 0
    then List.rev lst
    else aux (pred n) (pred cnt) ((fact (pred n) / fact (num - r)) :: lst)
  in
  aux num r []
;;

let find_nodes lst r idx =
  let rec aux elem_lst result idx = function
    | [] -> result
    | x :: xs ->
      let n = List.nth_exn elem_lst (idx / x) in
      aux (List.filter elem_lst ~f:(( <> ) n)) (n :: result) (idx mod x) xs
  in
  let lst_length = List.length lst in
  if idx < 0 || idx >= fact lst_length / fact (lst_length - r)
  then failwith "out of range"
  else aux lst [] idx (make_factorial_tbl lst_length r)
;;

let compute idx =
  find_nodes [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ] 10 (pred idx)
  |> List.rev
  |> List.fold ~init:"" ~f:(fun acc n -> acc ^ string_of_int n)
;;

let solve () = compute 1_000_000

(* Test *)

let%test_unit "1_000_000" = [%test_eq: string] (compute 1_000_000) "2783915460"
