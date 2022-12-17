(* Project Euler: Problem 38 *)

(*
  It is clear that number X is within 4 digits from the requirement
  1) if number X is four digits, n = 2  (X * 1 -> 4-digits, X * 2 -> 5-digits)
  2) if number X is three digits, n = 3  (X * 1 -> 3-digits, X * 2 -> 3-digits, X * 3 -> 3-digits)
  3) if number X is two digits, n = 4  (X * 1 -> 2-digits, X * 2 -> 2-digits, X * 3 -> 2-digits, X * 4 -> 3-digits)
  4) if number X is one digit, n = 9 or 5 (only X=1 and n=9, X=9 and n=5).

  case #1:
    5000 <= X <= 9999
  case #2:
    100 <= X <= 333
  case #3:
    10 <= X <= 33
  case #4:
    X = 1, 9
 *)

open Core

let rec find_candidates start stop =
  if start < stop then
    find_candidates stop start
  else
    let make_cand num =
      match num with
      | x when x = 1 -> Some [1; 2; 3; 4; 5; 6; 7; 8; 9]    (* case #4 *)
      | x when x = 9 -> Some [9; 18; 27; 36; 45]    (* case #4 *)
      | x when x >= 10 && x <= 33 -> Some [num; num * 2; num * 3; num * 4]    (* case #3 *)
      | x when x >= 1000 && x <= 333 -> Some [num; num * 2; num * 3]    (* case #2 *)
      | x when x >= 5000 && x <= 9999 -> Some [num; num * 2]    (* case #1 *)
      | _ -> None
    in
    let rec aux i result =
      if i < stop then
        result
      else
        match make_cand i with
        | Some lst -> if Euler.Math.is_pandigital_lst lst then
                        aux (pred i) ((Int.of_string (List.fold lst ~init:"" ~f:(fun acc n -> acc ^ Int.to_string n)), i) :: result)
                      else
                        aux (pred i) result
        | None -> aux (pred i) result
    in
    aux start []

let solve () =
  ((find_candidates 9999 5000) @ (find_candidates 333 100) @ (find_candidates 33 10) @ (find_candidates 9 9) @ (find_candidates 1 1))
  |> List.sort ~compare:(fun (p1, _) (p2, _) -> p2 - p1)
  |> List.hd_exn

let exec () =
  let prod, x = solve () in
  sprintf "%d (x=%d)" prod x

let () = Euler.Task.run exec
