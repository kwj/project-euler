(* Project Euler: Problem 1 *)

let make_seq upper =
  List.filter (fun n -> n mod 3 = 0 || n mod 5 = 0) (List.init upper (fun n -> n));;

let rec sum_of_seq l =
  match l with
  | [] -> 0
  | h :: rest -> h + (sum_of_seq rest);;

let () =
  Printf.printf "sum of all the multiples of 3 or 5 below 1000 is %d\n" (sum_of_seq (make_seq 1000));;
