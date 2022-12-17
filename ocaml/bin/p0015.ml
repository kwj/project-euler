(* Project Euler: Problem 15 *)

open Core

(*
  Method 1: combination
    (m+n)! / (m! * n!)
*)
let calc_by_zarith num =
  let open Z in
  to_string (div (fac (Int.(+) num num)) (mul (fac num) (fac num)))

(*
  Method 2: counting up

  [sample: matrix size=3]
    1 - 1 - 1 - 1
    |   |   |   |
    1 - 2 - 3 - 4
    |   |   |   |
    1 - 3 - 6 - 10
    |   |   |   |
    1 - 4 - 10 -20(End)

  # extend_list [1] 1;;
  - : int list = [1; 1]
  # extend_list [1] 2;;
  - : int list = [1; 2; 1]
  # extend_list [1] 3;;
  - : int list = [1; 3; 3; 1]
 *)
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
      List.hd_exn lst
    else
      shrink_list (aux lst)
  in
  string_of_int (shrink_list @@ extend_list [1] num)

let exec () =
  sprintf "%s (w/ Zarith module)\n%s (w/o Zarith module)" (calc_by_zarith 20) (calc_by_list 20)

let () = Euler.Task.run exec
