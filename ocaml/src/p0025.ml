(* Project Euler: Problem 25 *)

open Core

let fibs a b =
  Sequence.unfold
    ~init:(Z.of_int a, Z.of_int b)
    ~f:(fun (a, b) -> Some (a, (b, Z.(a + b))))
;;

let compute n_digits =
  let thr = Z.pow (Z.of_int 10) (n_digits - 1) in
  fibs 1 1
  |> Sequence.find_mapi ~f:(fun i v -> if Z.geq v thr then Some (succ i) else None)
  |> Option.value_exn
;;

let solve () = compute 1_000 |> Int.to_string

(* Test *)

let%test_unit "1000-digit" = [%test_eq: int] (compute 1_000) 4782
