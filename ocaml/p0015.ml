(* Project Euler: Problem 15 *)

let calc_by_zarith num =
  (* Answer: (m+n)! / (m! * n!) *)
  let open Z in
  to_string (div (fac (Int.add num num)) (mul (fac num) (fac num)))

let calc_by_list num =
  let rec extend_list lst n =
    let rec aux lst =
      match lst with
      | a :: b :: [] -> [(a + b); b]
      | a :: (b :: _ as tl) -> (a + b) :: (aux tl)
      | _ -> assert false
    in
    if n = 0 then
      lst
    else
      extend_list (aux (0 :: lst)) (pred n)
  in
  let rec shrink_list lst =
    let rec aux lst =
      match lst with
      | a :: b :: [] -> [a + b]
      | a :: (b :: _ as tl) -> (a + b) :: (aux tl)
      | [a] -> [a]
      | _ -> assert false
    in
    if List.length lst = 1 then
      List.hd lst
    else
      shrink_list (aux lst)
  in
  string_of_int (shrink_list @@ extend_list [1] num)

let () =
  Printf.printf "the number of routes is there through a 20×20 grid is:\n";
  Printf.printf "   %s: (w/ Zarith module)\n" (calc_by_zarith 20);
  Printf.printf "   %s: (w/o Zarith module)\n" (calc_by_list 20);
