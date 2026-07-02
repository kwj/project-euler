(* Project Euler: Problem 52 *)

open Core

let check_num n =
  let make_key x = Euler.Util.digits x |> List.sort ~compare:Int.compare in
  let key = make_key n in
  List.(
    range 2 6 ~stop:`inclusive
    |> for_all ~f:(fun i -> equal Int.equal key (make_key (n * i))))
;;

let compute () =
  Sequence.(
    unfold ~init:6 ~f:(fun n -> Some (n, n + 1))
    |> find_map ~f:(fun exp ->
      range (Int.pow 10 (exp - 1)) (Int.pow 10 exp / 6) ~stop:`inclusive
      |> find ~f:check_num)
    |> Option.value_exn)
;;

let solve () = compute () |> Int.to_string

(* Test *)

let%test_unit "Permuted Multiples" = [%test_eq: int] (compute ()) 142857
