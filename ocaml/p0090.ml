(* Project Euler: Problem 90 *)

(*
  01
  04
  09,06 - 0,(6,9)
  16,19 - 1,(6,9)
  25
  36,39 - 3,(6,9)
  49,46 - 4,(6,9)
  64,94 - 4,(6,9)
  81

  {2,5}, {1,8}
  {0,1}, {0,4}
  x=6|9: {x,0}, {x,1}, {x,3}, {x,4}

  case 1:
    [2, 1, #, #, #, #]
    [5, 8, x, #, #, #]

  case 2:
    [2, 8, x, #, #, #]
    [5, 1, #, #, #, #]

  nCr(n=10,c=6) = 210
  let's brute force.
 *)

(* ---------------------------------------------------------------- *)

let check_conditions lst1 lst2 =
  let check_1258 () =
    (List.mem 1 lst1 && List.mem 2 lst1 && List.mem 5 lst2 && List.mem 8 lst2)
    || (List.mem 5 lst1 && List.mem 8 lst1 && List.mem 1 lst2 && List.mem 2 lst2)
    || (List.mem 2 lst1 && List.mem 8 lst1 && List.mem 5 lst2 && List.mem 1 lst2)
    || (List.mem 5 lst1 && List.mem 1 lst1 && List.mem 2 lst2 && List.mem 8 lst2)
  in
  let check_69 n =
    (List.mem n lst1 && (List.mem 6 lst2 || List.mem 9 lst2)) ||
      (List.mem n lst2 && (List.mem 6 lst1 || List.mem 9 lst1))
  in
  let check_0 n =
    (List.mem n lst1 && List.mem 0 lst2) ||
      (List.mem n lst2 && List.mem 0 lst1)
  in
  let check f lst =
    let rec loop = function
      | [] -> true
      | hd :: tl -> if f hd then loop tl else false
    in
    loop lst
  in
  if check_1258() then
    if check check_69 [0; 1; 3; 4] then
      if check check_0 [1; 4] then
        true
      else
        false
    else
      false
  else
    false

let check_lst lst1 lst2 =
  let rec loop1 lst acc =
    match lst with
    | [] -> acc
    | hd :: tl -> 
       let rec loop2 l1 lst acc =
         match lst with
         | [] -> acc
         | hd :: tl ->
            if check_conditions l1 hd then (
              loop2 l1 tl (succ acc)
            ) else
              loop2 l1 tl acc
       in
       loop1 tl (acc + (loop2 hd lst2 0))
  in
  loop1 lst1 0

let solve () =
  let rec comb r lst =
    match r, lst with
    | 0, _ -> [[]]
    | _, []  -> []
    | n, hd :: tl -> List.map (List.cons hd) (comb (n - 1) tl) @ comb n tl
  in
  let dice_lst = comb 6 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  (check_lst dice_lst dice_lst) / 2

let () =
  Printf.printf "Answer: %d\n" (solve ())

