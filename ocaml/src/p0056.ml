(* Project Euler: Problem 56 *)

open Core

let compute () =
  List.range 2 100 ~stop:`inclusive
  |> List.filter ~f:(fun n -> n mod 10 <> 0)
  |> List.map ~f:(fun n ->
    let a = Z.of_int n in
    let rec loop ans prod cnt =
      if cnt = 0
      then ans
      else
        if (String.length (Z.to_string prod)) * 9 < ans
        then ans
        else (
          loop
            (Int.max (List.sum (module Int) ~f:Fn.id (Euler.Util.z_digits prod)) ans)
            Z.(prod / a)
            (pred cnt))
    in
    loop 0 Z.(pow a 100) 100)
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn
;;

let solve () = compute () |> Int.to_string

(* Test *)

let%test_unit "Powerful Digit Sum" = [%test_eq: int] (compute ()) 972
