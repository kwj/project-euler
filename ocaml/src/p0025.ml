(* Project Euler: Problem 25 *)

open Core

let fibs a b =
  Sequence.unfold
    ~init:(Z.of_int a, Z.of_int b, 1)
    ~f:(fun (a, b, idx) -> Some ((idx, a), (b, Z.(a + b), succ idx)))
;;

let compute n_digits =
  let thr = Z.pow (Z.of_int 10) (n_digits - 1) in
  fibs 1 1
  |> Sequence.drop_while ~f:(fun tpl -> Z.lt (snd tpl) thr)
  |> Sequence.hd_exn
  |> fst
;;

let solve () = compute 1_000 |> Int.to_string

(* Test *)

let%test_unit "1000-digit" = [%test_eq: int] (compute 1_000) 4782
