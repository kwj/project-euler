(* Project Euler: Problem 49 *)

open Core

let get_prime_tbl n_digits =
  let make_key n = Euler.Util.digits n |> List.sort ~compare:Int.compare |> Euler.Util.undigits in

  Euler.Math.Prime.primes (Int.pow 10 (n_digits - 1)) (Int.pow 10 n_digits)
  |> List.filter_map ~f:(fun n -> if n >= 1000 then Some (make_key n, n) else None)
  |> Hashtbl.of_alist_multi ~growth_allowed:false (module Int)
;;

let compute n_digits =
  List.(
    get_prime_tbl n_digits
    |> Hashtbl.to_alist
    |> filter_map ~f:(fun (_, lst) ->
      if length lst < 3
      then None
      else (
        let cands =
          Euler.Util.combination 2 lst
          |> filter_map ~f:(fun l ->
            let tmp = (nth_exn l 0 * 2) - nth_exn l 1 in
            if exists lst ~f:(fun n -> n = tmp)
            then Some [ tmp; nth_exn l 0; nth_exn l 1 ]
            else None)
        in
        if length cands > 0 then Some cands else None))
    |> concat
    |> map ~f:(Fun.compose (reduce_exn ~f:( ^ )) (rev_map ~f:Int.to_string))
    |> filter ~f:(fun s -> String.compare s "148748178147" <> 0)
    |> hd_exn)
;;

let solve () = compute 4

(* Test *)

let%test_unit "4" = [%test_eq: string] (compute 4) "296962999629"
