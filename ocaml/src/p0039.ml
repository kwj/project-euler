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

let check_pair p a = ((p * p) - (2 * a * p)) mod (2 * (p - a)) = 0

let compute limit =
  let rec perim_loop res = function
    | [] -> res
    | p :: ps ->
      let rec aux lst = function
        | [] -> lst
        | a :: xs ->
          if check_pair p a
          then (
            let b = ((p * p) - (2 * a * p)) / (2 * (p - a)) in
            aux ((a, b, p - a - b) :: lst) xs)
          else aux lst xs
      in
      (match List.(aux [] (range 1 ((p - 1) / 3) ~stop:`inclusive) |> length) with
       | 0 -> perim_loop res ps
       | len -> perim_loop ((len, p) :: res) ps)
  in
  perim_loop [] (List.range 2 limit ~stop:`inclusive ~stride:2)
  |> List.max_elt ~compare:(fun (x, _) (y, _) -> Int.compare x y)
  |> Option.value_exn
  |> snd
;;

let solve () = compute 1_000 |> Int.to_string

(* Test *)

let%test_unit "1000" = [%test_eq: int] (compute 1_000) 840
