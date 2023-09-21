(* Project Euler: Problem 49 *)

open Core

let get_prime_tbl n_digits =
  let tbl = Hashtbl.create (module Int) in
  Euler.Math.Prime.primes (Int.pow 10 (n_digits - 1)) (Int.pow 10 n_digits)
  |> List.filter ~f:(fun n -> n >= 1000)
  |> List.iter ~f:(fun p ->
    let key = Euler.Util.digits p |> List.sort ~compare |> Euler.Util.undigits in
    Hashtbl.update tbl key ~f:(fun v ->
      match v with
      | None -> [p]
      | Some lst -> p :: lst));
  tbl
;;

let compute n_digits =
  get_prime_tbl n_digits
  |> Hashtbl.to_alist
  |> List.filter_map ~f:(fun (_, lst) ->
    if List.length lst < 3
    then None
    else (
      let cands =
        Euler.Util.combination 2 lst
        |> List.filter_map ~f:(fun l ->
          let tmp = (List.nth_exn l 0 * 2) - List.nth_exn l 1 in
          if List.exists lst ~f:(fun n -> n = tmp)
          then Some [ tmp; List.nth_exn l 0; List.nth_exn l 1 ]
          else None)
      in
      if List.length cands > 0 then Some cands else None))
  |> List.concat
  |> List.map ~f:(fun lst ->
    List.fold ~init:"" ~f:(fun acc n -> (Int.to_string n) ^ acc) lst)
  |> List.filter ~f:(fun s -> String.compare s "148748178147" <> 0)
  |> List.hd_exn
;;

let solve () = compute 4

(* Test *)

let%test_unit "4" = [%test_eq: string] (compute 4) "296962999629"
