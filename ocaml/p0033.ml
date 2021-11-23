(* Project Euler: Problem 33 *)

(*
  case #1: (10a + c) / (10c + b) = a / b
  case #2: (10a + c) / (10b + c) = a / b
  case #3: (10c + a) / (10c + b) = a / b
  case #4: (10c + a) / (10b + c) = a / b
    // prerequisite: a < b

  #1:
      10ab + bc = 10ac + ab
   -> 9ab = 10ac - bc
   -> 9ab - 9ac = ac - bc
   -> 9a(b - c) = c(a - b)
   -> 9a(c - b) = c(b - a)  [a < b => a < b < c]
  #2:
      10ab + bc = 10ab + ac
   -> bc = ac
   -> b = a   ==> NG (contradiction)
  #3:
      10bc + ab = 10ac + ab
   -> 10bc = 10ac
   -> b = a   ==> NG (contradiction)
  #4:
      10bc + ab = 10ab + ac
   -> 10bc - ac = 9ab
   -> bc - ac = 9ab - 9bc
   -> c(b - a) = 9b(a - c)  [a < b => c < a < b]
   -> a - c = c/9 - ac/9b => 1   ==> NG (bacause c/9 < 1)

  so, I search for only case #1.
 *)

(* ---------------------------------------------------------------- *)

let rec combination r lst =
  match r, lst with
  | 0, _ -> [[]]
  | _, []  -> []
  | n, hd :: tl -> List.map (List.cons hd) (combination (n - 1) tl) @ combination n tl

let rec gcd m n =
  if n = 0 then m else gcd n (m mod n)

let solve () =
  let cands = combination 3 (List.init 9 (fun e -> e + 1)) in
  let rec aux lst result =
    match lst with
    | [] -> result
    | hd :: tl ->
       let a, b, c = List.nth hd 0, List.nth hd 1, List.nth hd 2 in
       if 9 * a * (c - b) = c * (b - a) then
         aux tl ((a, b) :: result)
       else
         aux tl result
  in
  let a, b = List.fold_left (fun (x1, x2) (y1, y2) -> (x1 * y1, x2 * y2)) (1, 1) (aux cands []) in
  b / (gcd a b)

let () =
  Printf.printf "Answer: %d\n" (solve())
