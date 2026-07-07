(* Project Euler: Problem 49 *)

open Core

let no_exist_msg = "There are no such sequences."

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
  assert (n_digits > 0 && n_digits <= 4);

  let result =
    List.(
      get_prime_group_lst n_digits
      |> filter_map ~f:(fun lst ->
        if length lst < 3 then None else Some (sort ~compare:Int.ascending lst))
      >>= Euler.Util.combination 3
      |> filter ~f:(fun triplet ->
        nth_exn triplet 1 - nth_exn triplet 0 = nth_exn triplet 2 - nth_exn triplet 1)
      |> map ~f:(Fun.compose (reduce_exn ~f:( ^ )) (map ~f:Int.to_string))
      |> filter ~f:(Fun.compose not (String.equal "148748178147")))
  in
  match List.length result with
  | 0 -> no_exist_msg
  | 1 -> List.hd_exn result
  | _ -> failwith "Error: There are two or more sequences."
;;

let solve () = compute 4

(* Test *)

let%test_unit "1" = [%test_eq: string] (compute 1) no_exist_msg
let%test_unit "2" = [%test_eq: string] (compute 2) no_exist_msg
let%test_unit "3" = [%test_eq: string] (compute 3) no_exist_msg
let%test_unit "4" = [%test_eq: string] (compute 4) "296962999629"
