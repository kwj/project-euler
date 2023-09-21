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

   Therefore, I only need to search for case #1.
*)

open Core

let compute () =
  let rec aux result = function
    | [] -> result
    | xs :: xss ->
      let a, b, c = (List.nth_exn xs 0, List.nth_exn xs 1, List.nth_exn xs 2) in
      if 9 * a * (c - b) = c * (b - a) then aux ((a, b) :: result) xss else aux result xss
  in
  let cands = Euler.Util.combination 3 [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ] in
  let a, b =
    List.fold (aux [] cands) ~init:(1, 1) ~f:(fun (x1, x2) (y1, y2) -> (x1 * y1, x2 * y2))
  in
  b / Euler.Math.gcd a b
;;

let solve () = compute () |> Int.to_string

(* Test *)

let%test_unit "digit cancelling fractions" = [%test_eq: int] (compute ()) 100
