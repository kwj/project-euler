(* Project Euler: Problem 100 *)

open Core

let compute thr =
  let limit = (2 * thr) - 1 in
  Sequence.unfold ~init:(1, 1) ~f:(fun (x, y) ->
    Some ((x, y), ((3 * x) + (4 * y), (2 * x) + (3 * y))))
  |> Sequence.find ~f:(fun (x, _) -> x > limit)
  |> Option.value_exn
  |> snd
  |> fun y -> (y + 1) / 2
;;

let solve () = compute 1_000_000_000_000 |> Int.to_string

(* Test *)

let%test_unit "1_000_000_000_000" =
  [%test_eq: int] (compute 1_000_000_000_000) 756872327473
;;
