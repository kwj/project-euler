(* Project Euler: Problem 5 *)

let rec pow b e =
  if e <= 0 then 1 else b * pow b (e - 1);;
    
let () =
  let numbers = List.init 100 (fun n -> n + 1) in
  let sum_list lst = List.fold_left (+) 0 lst in
  Printf.printf "the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum is %d\n" (abs (sum_list (List.map (fun n -> n * n) numbers) - (pow (sum_list numbers) 2)));;
