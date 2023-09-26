(* Project Euler: Problem 45 *)

(*
 * Hexagonal numbers are also triangle numbers.
 *
 *   H(k) = k(2k - 1)
 *        = 2k(2k - 1) / 2
 *        = T(2k - 1)
 *
 * We therefore search for hexagonal numbers which are also pentagonal numbers.
 *
 * P{i} = H{j}
 * i(3i - 1) / 2 = j(2j - 1)
 * 3i^2 - i = 4j^2 - 2j
 * (6i - 1)^2 - 1 = 3(4j - 1)^2 - 3
 * -->
 * (6i - 1)^2 - 3(4j - 1)^2 = -2
 *  ------        ------
 *     X            Y
 *
 * see https://imomath.com/index.cgi?page=ntPellsEquationPellType
 * -->
 * z0 = 2 + sqrt(3), z = 1 + sqrt(3)
 * X{n} + Y{n} sqrt(3) = (1 + sqrt(3)) (2 + sqrt(3))^n
 *
 * X{n} = 2X{n-1} + 3Y{n-1}
 * Y{n} = X{n-1} + 2Y{n-1}
 *   where X{0} = 1, Y{0} = 1
 *)

open Core

let compute nth =
  let _, y =
    Sequence.unfold ~init:(1, 1) ~f:(fun (a, b) ->
      Some ((a, b), ((2 * a) + (3 * b), a + (2 * b))))
    |> Sequence.filter ~f:(fun (a, b) -> a mod 6 = 5 && b mod 4 = 3)
    |> Fun.flip Sequence.nth_exn (nth - 1)
  in
  let j = (y + 1) / 4 in
  j * ((2 * j) - 1)
;;

let solve () = compute 3 |> Int.to_string

(* Test *)

let%test_unit "1st" = [%test_eq: int] (compute 1) 1
let%test_unit "2nd" = [%test_eq: int] (compute 2) 40755
let%test_unit "3rd" = [%test_eq: int] (compute 3) 1533776805
