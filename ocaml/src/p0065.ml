(* Project Euler: Problem 65 *)

(*
 * e = [2; 1, 2, 1, 1, 4, 1, 1, 6, 1, ...]
 *     [a{0}; a{1}, a{2}, ...]
 *
 *   i  a{i-1}  n(numerator)  d(denominator)
 *  ----------------------------------------
 *   1   2         2             1
 *   2   1         3             1
 *   3   2         8             3
 *   4   1        11             4
 *   5   1        19             7
 *   6   4        87            32
 *   7   1       106            39
 *   8   1       193            71
 *   9   6      1264           465
 *  10   1      1457           536
 *            ...
 *   i c(i)     n(i)          d(i)
 *
 *   when i > 2:
 *     n(i) = n(i-1)*c(i) + n(i-2), n(2) = 3, n(1) = 2
 *     d(i) = d(i-1)*c(i) + d(i-2), d(2) = 1, d(1) = 1
 *
 *     c(i) = | 1    (i mod 3 <> 0)
 *            | 2n/3 (i mod 3 = 0)
 *)

open Core

let c = function
  | n when n mod 3 = 0 -> Z.of_int (2 * n / 3)
  | _ -> Z.one
;;

let compute stop =
  let start = 3 in
  assert (stop >= start);

  Sequence.unfold
    ~init:(Z.(~$3), Z.(~$2), start)
    ~f:(fun (a, b, idx) -> Some (Z.((a * c idx) + b), (Z.((a * c idx) + b), a, succ idx)))
  |> Fn.flip Sequence.nth_exn (stop - start)
  |> Euler.Util.z_digits
  |> List.sum (module Int) ~f:Fn.id
;;

let solve () = compute 100 |> Int.to_string

(* Test *)

let%test_unit "10" = [%test_eq: int] (compute 10) 17
let%test_unit "100" = [%test_eq: int] (compute 100) 272
