(* Project Euler: Problem 56 *)

open Core

let find_max_digital_sum answer a b_max =
  let rec aux acc pow_ab =
    let pow_ab_lst = Euler.Util.z_digits pow_ab in
    if List.length pow_ab_lst * 9 < acc || Z.(equal pow_ab one)
    then acc
    else aux (Int.max (List.reduce_exn ~f:( + ) pow_ab_lst) acc) Z.(pow_ab / a)
  in
  aux answer (Z.pow a b_max)
;;

let compute upper =
  let a_lst =
    List.(
      range upper 2 ~stride:(-1) ~stop:`inclusive
      |> filter ~f:(fun n -> n mod 10 <> 0)
      |> map ~f:Z.of_int)
  in
  let rec aux ans = function
    | [] -> ans
    | x :: xs -> aux (find_max_digital_sum ans x upper) xs
  in
  aux 0 a_lst
;;

let solve () = compute 99 |> Int.to_string

(* Test *)

let%test_unit "Powerful Digit Sum" = [%test_eq: int] (compute 99) 972
