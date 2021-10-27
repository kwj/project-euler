(* Project Euler: Problem 5 *)

let rec gcd m n =
  (* Euclidean algorithm *)
  if n <> 0 then gcd n (m mod n) else abs m;;

let lcm m n =
  match m, n with
  | 0, _ | _, 0 -> 0
  | m, n -> abs (m * n) / (gcd m n);;

let lcm_list lst =
  List.fold_left lcm 1 lst;;

let () =
  Printf.printf "the smallest positive number that is evenly divisible by all of the numbers from 1 to 20 is %d\n" (lcm_list (List.init 20 (fun n -> n + 1)));;
