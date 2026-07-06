(* Project Euler: Problem 49 *)

open Core

let get_prime_group_lst n_digits =
  let make_key n =
    Euler.Util.(digits n |> List.sort ~compare:Int.ascending |> undigits)
  in

  Euler.Math.Prime.primes (Int.pow 10 (n_digits - 1)) (Int.pow 10 n_digits)
  |> List.map ~f:(fun n -> (make_key n, n))
  |> Euler.Util.list_assoc_group
  |> List.map ~f:snd
;;

let compute n_digits =
  assert (n_digits > 0);

  List.(
    get_prime_group_lst n_digits
    |> filter_map ~f:(fun lst ->
      if length lst < 3 then None else Some (sort ~compare:Int.ascending lst))
    >>= Euler.Util.combination 3
    |> filter ~f:(fun lst ->
      nth_exn lst 1 - nth_exn lst 0 = nth_exn lst 2 - nth_exn lst 1)
    |> map ~f:(Fun.compose (reduce_exn ~f:( ^ )) (map ~f:Int.to_string))
    |> filter ~f:(fun s -> String.compare s "148748178147" <> 0)
    |> hd
    |> function
    | None -> "There are no such sequences."
    | Some s -> s)
;;

let solve () = compute 4

(* Test *)

let%test_unit "4" = [%test_eq: string] (compute 4) "296962999629"
