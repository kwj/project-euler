(* Project Euler: Problem 2 *)

let rec make_fib_seq upper l =
  match l with
  | [] -> make_fib_seq upper [1; 0]
  | m :: n :: _ -> if m + n >= upper then l else make_fib_seq upper (m + n :: l)
  | _ -> l;;

let () =
  Printf.printf "sum of the even-valued terms of the Fibonacci sequence is %d\n"
    (List.fold_left (+) 0 (List.filter (fun n -> n mod 2 = 0) (make_fib_seq 4_000_000 [])))
