(* Project Euler: Problem 64 *)

(*
 *           sqrt(N) + b0        1              1
 * sqrt(N) = ------------ = a0 + --,  x1 = a1 + --, ...
 *                c0             x1             x2
 *
 *                 c0             c0(sqrt(N) - (b0 - a0c0))
 *   x1 = --------------------- = -------------------------
 *        sqrt(N) + (b0 - a0c0)       N - (b0 - a0c0)^2
 *
 *        sqrt(N) + (a0c0 - b0)   sqrt(N) + b1         1
 *      = --------------------- = ------------- = a1 + --
 *          N - (a0c0 - b0)^2          c1              x2
 *          -----------------
 *                 c0
 *  -->
 *    a{n} = floor( (sqrt(N)+b{n}) / c{n} ) = (floor(sqrt(N) + b{n}) / c{n}
 *    b{n+1} = a{n}*c{n} - b{n}
 *    c{n+1} = (N - b{n+1}^2) / c{n}
 *
 *    a{0} = sqrt(N), b{0} = 0, c{0} = 1
 *
 *  -->
 *    cycle_lst = [((b{n}, c{n}), n); ...; ((b{1}, c{1}), 1); ((b{0}, c{0}), 0)]
 *)

open Core

let get_cont_fraction n =
  let isqrt_n = Euler.Math.isqrt n in
  if Int.pow isqrt_n 2 = n
  then (isqrt_n, [])
  else (
    let stop_condition = isqrt_n * 2 in
    let rec loop a b c lst =
      let b = (a * c) - b in
      let c = (n - (b * b)) / c in
      let a = (isqrt_n + b) / c in
      if a = stop_condition then (isqrt_n, List.rev (a :: lst)) else loop a b c (a :: lst)
    in
    loop isqrt_n 0 1 [])
;;

let compute limit =
  List.range 1 limit ~stop:`inclusive
  |> List.count ~f:(fun n ->
    let _, lst = get_cont_fraction n in
    List.length lst mod 2 = 1)
;;

let solve () = compute 10_000 |> Int.to_string

(* Test *)

let%test_unit "13" = [%test_eq: int] (compute 13) 4
let%test_unit "10_000" = [%test_eq: int] (compute 10_000) 1322
