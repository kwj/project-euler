(* Project Euler: Problem 92 *)

open Core

let rec is_group89 n =
  if n <> 89 && n > 1
  then (
    let rec aux n acc =
      if n <> 0
      then aux (n / 10) (acc + Int.pow (n mod 10) 2)
      else acc
    in
    is_group89 (aux n 0))
  else n = 89
;;

let rec factorial n =
  if n > 1 then n * factorial (n - 1) else 1
;;

let countmap t lst =
  let ret = Hashtbl.create t in
  List.iter lst ~f:(fun elm -> Hashtbl.update ret elm ~f:(fun v ->
    match v with
    | None -> 1
    | Some n -> n + 1));
  ret
;;

let compute limit =
  assert (Euler.Math.num_of_digits limit = Euler.Math.num_of_digits (limit - 1) + 1);
  let n_digits = Euler.Math.num_of_digits (limit - 1) in
  let numer = factorial n_digits in

  List.range 0 9 ~stop:`inclusive
  |> List.map ~f:(fun n -> n * n)
  |> Euler.Util.combination_with_repetition n_digits
  |> List.filter ~f:(fun lst -> is_group89 (List.sum (module Int) ~f:Fn.id lst))
  |> List.map ~f:(fun lst ->
    Hashtbl.fold (countmap (module Int) lst) ~init:1 ~f:(fun ~key:_ ~data:v acc -> acc * (factorial v)))
  |> List.map ~f:(fun denom -> numer / denom)
  |> List.sum (module Int) ~f:Fn.id
;;

let solve () = compute 10_000_000 |> Int.to_string

(* Test *)

let%test_unit "10" = [%test_eq: int] (compute 10) 7
let%test_unit "10_000_000" = [%test_eq: int] (compute 10_000_000) 8581146
