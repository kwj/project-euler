(* Project Euler: Problem 31 *)

let solve_dp coins target =
  let table = Array.make (target + 1) 0 in
  let rec aux lst =
    match lst with
    | [] -> table.(target)
    | hd :: tl ->
       for i = hd to target do
         table.(i) <- table.(i) + table.(i - hd)
       done;
       aux tl
  in
  table.(0) <- 1;
  aux coins

let () =
  Printf.printf "How many different ways can 200p be made using any number of coins? (coins: 1p, 2p, 5p, 10p, 20p, 50p, 100p, 200p)\n";
  Printf.printf "  Answer: %d\n" (solve_dp [1; 2; 5; 10; 20; 50; 100; 200] 200)
