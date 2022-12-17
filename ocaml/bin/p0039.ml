(* Project Euler: Problem 39 *)

(*
  assume that a <= b < c, a + b + c = p ==> a < p/3

  a^2 + b^2 = (p - a - b)^2
  => a^2 + b^2 = p^2 -2ap - 2bp + a^2 + 2ab + b^2
  => p^2 -2ap - 2bp + 2ab = 0
  => 2bp - 2ab = p^2 - 2ap
  => 2b(p - a) = p^2 - 2ap
  => b = (p^2 - 2ap) / 2(p - a)

     a  b  p  (E:even, O:odd)
   -----------
     E  E  E
     E  E  O  ==> NG (contradiction, c is even because c^2 = a^2 + b^2 is even.)
     E  O  E
     E  O  O  ==> NG (contradiction, c is odd because c^2 = a^2 + b^2 is odd.)
     O  E  E
     O  E  O  ==> NG (contradiction, c is odd because c^2 = a^2 + b^2 is odd.)
     O  O  E
     O  O  O  ==> NG (contradiction, c is even because c^2 = a^2 + b^2 is even.)

    'p' is always EVEN.
 *)

open Core

let solve num =
  let rec find_candidates p result =
    let rec aux a result =
      if a <= 0 then
        result
      else
        if (p * p - 2 * a * p) mod (2 * (p - a)) = 0 then
          let b = (p * p - 2 * a * p) / (2 * (p - a)) in
          aux (pred a) ((a, b, p - a - b) :: result)
        else
          aux (pred a) result
    in
    if p <= 0 then
      result
    else
      let cands = aux (p / 3) [] in
      if List.length cands > 0 then
        find_candidates (p - 2) ((cands, p) :: result)
      else
        find_candidates (p - 2) result
  in
  let start_p = if num mod 2 = 0 then num else num - 1 in

  find_candidates start_p []
  |> List.sort ~compare:(fun (t1, _) (t2, _) -> List.length t2 - List.length t1)
  |> List.hd_exn

let exec () =
  let lst, p = solve 1_000 in
  sprintf "%d (number of solutions = %d)" p (List.length lst)

let () = Euler.Task.run exec
