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

(* ---------------------------------------------------------------- *)


let solve num =
  let rec aux p result =
    let rec search_sols a sols =
      if a <= 0 then
        sols
      else
        if (p * p - 2 * a * p) mod (2 * (p - a)) = 0 then
          let b = (p * p - 2 * a * p) / (2 * (p - a)) in
          search_sols (pred a) ((a, b, p - a - b) :: sols)
        else
          search_sols (pred a) sols
    in
    if p <= 0 then
      result
    else
      let tmp = search_sols (p / 3) [] in
      if List.length tmp > 0 then
        aux (p - 2) ((tmp, p) :: result)
      else
        aux (p - 2) result
  in
  let start = if num mod 2 = 0 then num else num - 1 in
  List.hd @@ List.sort (fun (t1, _) (t2, _) -> List.length t2 - List.length t1) (aux start [])

let () =
  let _, p = solve 1000 in
  Printf.printf "Answer: %d\n" p
